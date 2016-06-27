(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*s From MiniML to ocaml internal Lambda AST *)

open Util
open Names
open Globnames
open Miniml
open Extraction_plugin

open Lambda (* in compiler-libs *)

(** Basic data representation *)

let mkint n = Lconst (Const_base (Asttypes.Const_int n))
let mkchar c = Lconst (Const_base (Asttypes.Const_char c))
let mkstring str = Lconst (Const_base (Asttypes.Const_string str))
let mkblock tag args = Lprim (Pmakeblock (tag, Asttypes.Immutable), args)
let mkexn str = mkblock 0 [mkstring str]

(* (lazy t) is a block with (fun _ -> t) as first field *)
let mklazy t =
  mkblock Obj.lazy_tag [Lfunction (Curried, [Ident.create "lazy"], t)]

(* For Lazy.force, we avoid the Plazyforce primitive which seems
   almost unused in the OCaml compiler, and use directly [inline_lazy_force]
   instead. NB: for native code, beware of the Clflags.native_code used
   in inline_lazy_force *)
let mkforce t = Matching.inline_lazy_force t Location.none

let id_of_id id = Ident.create (Id.to_string id)
let id_of_mlid id = id_of_id (Mlutil.id_of_mlid id)


(** Implementation of dummy *)

let reset_dummy, get_dummy_name, add_dummy_def =
  let var = ref (Ident.create "dummy") in
  let used = ref false in
  (fun () -> var := Ident.create "dummy"; used := false),
  (fun () -> used := true; !var),
  (fun t ->
    if not !used then t
    else
      Lletrec ([!var,Lfunction (Curried,[Ident.create "x"],Lvar !var)],t))


(** Table of global names *)

let global_table = ref (Refmap_env.empty : Ident.t Refmap_env.t)

let id_of_global r =
  if Table.is_inline_custom r then failwith "Custom : unsupported";
  try Refmap_env.find r !global_table
  with Not_found ->
    assert (isConstRef r);
    let id = Ident.create (Constant.to_string (destConstRef r)) in
    global_table := Refmap_env.add r id !global_table;
    id


(** Table of inductives and constructors *)

type ind_info =
  { ind_tags : Types.constructor_tag array;
    ind_consts : constructor array; (* constant constructors, by index *)
    ind_nonconsts : constructor array } (* non-csts constructors, by tags *)

let ind_table = ref (Indmap_env.empty : ind_info Indmap_env.t)

let get_mlind (kn,i) =
  let mi = Extraction.extract_inductive (Global.env ()) kn in
  mi.ind_packets.(i)

let get_ind_info ind =
  try Indmap_env.find ind !ind_table
  with Not_found ->
    let nb_const = ref 0 and nb_block = ref 0 in
    let tags = Array.map
      (function
      | [] -> let n = !nb_const in incr nb_const; Types.Cstr_constant n
      | _ -> let n = !nb_block in incr nb_block; Types.Cstr_block n)
      (get_mlind ind).ip_types
    in
    let consts = Array.make !nb_const (ind,0) in
    let nonconsts = Array.make !nb_block (ind,0) in
    let () = Array.iteri
      (fun i tag -> match tag with
      | Types.Cstr_constant n -> consts.(n) <- (ind,i+1)
      | Types.Cstr_block n -> nonconsts.(n) <- (ind,i+1)
      | _ -> assert false)
      tags
    in
    let info = {
      ind_tags = tags;
      ind_consts = consts;
      ind_nonconsts = nonconsts }
    in
    ind_table := Indmap_env.add ind info !ind_table;
    info

let get_cons_tag = function
  |ConstructRef (ind,j) -> (get_ind_info ind).ind_tags.(j-1)
  |_ -> assert false

let reset_tables () =
  global_table := Refmap_env.empty;
  ind_table := Indmap_env.empty

(** Record fields *)

let projection_rank ind r =
  let fields = Table.get_record_fields (IndRef ind) in
  let rec search i = function
    | [] -> raise Not_found
    | Some f :: _ when Globnames.eq_gr f r -> i
    | _ :: l -> search (i+1) l
  in
  search 0 fields


(** Local environment for variables *)

let get_db_name n db =
  let id = List.nth db (pred n) in
  id
  (* TODO if Id.equal id dummy_name then Id.of_string "__" else id *)

let push_vars ids db = ids @ db


(** Compilation of patterns *)

type env = Ident.t list

(** [finalize_branch] adapts a branch [C x1...xn => t]
    (where [t] is lifted by [n]) to a code such as
    [let x1 = proj1 head in ... let xn = projn head in (cont t)] *)

let finalize_branch env (cont:env->ml_ast->lambda) id_head (ids,_,trm) =
  let rec loop env k = function
    | [] -> cont env trm
    | id::ids ->
      let id' = id_of_mlid id in
      let env' = push_vars [id'] env in
      Llet (Alias, id', Lprim (Pfield k, [Lvar id_head]),
            loop env' (k+1) ids)
  in
  loop env 0 ids

(** [do_branches] turns all the branches of a MLcase into lambda codes,
    that are organized as lists of constant branches, block branches, and
    an optional failaction. *)

let do_branches env (cont:env->ml_ast->lambda) id_head br =
  let csts = ref [] and blks = ref [] and ends = ref None in
  let do_branch ((ids,patt,trm) as branch) = match patt with
    |Pusual r ->
      let code = finalize_branch env cont id_head branch in
      (match get_cons_tag r with
      | Types.Cstr_constant i -> csts := (i,code) :: !csts
      | Types.Cstr_block i -> blks := (i,code) :: !blks
      | _ -> assert false)
    |Pwild ->
      assert (List.is_empty ids);
      assert (Option.is_empty !ends);
      ends := Some (cont env trm)
    |Prel 1 ->
      assert (List.length ids = 1);
      assert (Option.is_empty !ends);
      let env' = push_vars [id_head] env in (* hack : we reuse id_head *)
      ends := Some (cont env' trm)
    |_ -> failwith "Pattern currently unsupported"
  in
  Array.iter do_branch br;
  (List.rev !csts, List.rev !blks, !ends)

(** [mkswitch] builds a [Lswitch] out of lists of constant branches,
    block branches and optional failaction. It also tries to produce
    shorter code when possible (e.g. Lifthenelse). *)

let mkswitch ind_info id_head = function
  (* zero branch *)
  | [], [], None -> assert false (* this case should have become a MLexn *)
  (* one branch *)
  | [(_,e)], [], None
  | [], [(_,e)], None
  | [], [], Some e -> e
  (* two branches with at least a constant one *)
  | [(i1,e1);(_,e2)], [], None
  | [(i1,e1)], [_,e2], None
  | [(i1,e1)], [], Some e2 ->
    if Int.equal i1 0 then
      Lifthenelse (Lvar id_head, e2, e1) (* NB: default test is non-nullity *)
    else
      let test = Lprim (Pintcomp Ceq,[Lvar id_head; mkint i1]) in
      Lifthenelse (test, e1, e2)
  (* otherwise a general switch *)
  | csts,blks,last ->
    let sw =
      {sw_numconsts = Array.length ind_info.ind_consts;
       sw_consts = csts;
       sw_numblocks = Array.length ind_info.ind_nonconsts;
       sw_blocks = blks;
       sw_failaction = last}
    in
    Lswitch (Lvar id_head, sw)


(** Compilation of terms *)

let apply e = function
  | [] -> e
  | args -> Lapply (e,args,Location.none)

let rec do_expr env args = function
  |MLrel n -> let id = get_db_name n env in apply (Lvar id) args
  |MLapp (f,args') ->
    let stl = List.map (do_expr env []) args' in
    do_expr env (stl @ args) f
  |MLlam _ as a ->
    let fl,a' = Mlutil.collect_lams a in
    let fl = List.map id_of_mlid fl in
    let env' = push_vars fl env in
    let st = Lfunction (Curried, List.rev fl, do_expr env' [] a') in
    apply st args
  |MLletin (id,a1,a2) ->
    let id' = id_of_mlid id in
    let env' = push_vars [id'] env in
    let e1 = do_expr env [] a1 in
    let e2 = do_expr env' [] a2 in
    (* Warning: "Alias" below only holds for pure extraction *)
    apply (Llet (Alias, id', e1, e2)) args
  |MLglob r ->
    (try
       let ind, arity = Table.projection_info r in
       if List.length args < arity then raise Not_found;
       match List.skipn arity args with
       | [] -> raise Not_found
       | blk::args -> apply (Lprim (Pfield (projection_rank ind r),[blk])) args
     with Not_found -> apply (Lvar (id_of_global r)) args)
  |MLcons (_,r,a) as c ->
    assert (List.is_empty args);
    if Common.is_native_char c then mkchar (Common.get_native_char c)
    else
      let t = match get_cons_tag r with
        | Types.Cstr_constant n -> assert (List.is_empty a); mkint n
        | Types.Cstr_block tag -> mkblock tag (List.map (do_expr env []) a)
        | _ -> assert false
      in
      if Table.is_coinductive r then mklazy t else t
  |MLcase (typ, t, pv) ->
    if Table.is_custom_match pv then failwith "unsupported custom match";
    let ind = match Mlutil.type_simpl typ with
      | Tglob (IndRef i,_) -> i
      | _ -> assert false
    in
    let info = get_ind_info ind in
    let head = do_expr env [] t in
    let head = if Table.is_coinductive_type typ then mkforce head else head in
    let id = Ident.create "sw" in
    let branches = do_branches env (fun e -> do_expr e []) id pv in
    Llet (Alias, id, head, apply (mkswitch info id branches) args)
  |MLfix (i,ids,defs) ->
    let rev_ids = List.rev_map id_of_id (Array.to_list ids) in
    let env' = push_vars rev_ids env in
    let ids' = List.rev rev_ids in
    Lletrec
      (List.map2 (fun id t -> id, do_expr env' [] t) ids' (Array.to_list defs),
       apply (Lvar (List.nth ids' i)) args)
  |MLexn s -> Lprim (Praise, [mkblock 0 [mkexn s]])
  |MLdummy _ -> Lvar (get_dummy_name ()) (* TODO: could frequently be () *)
  |MLmagic a -> do_expr env args a
  |MLaxiom -> failwith "An axiom must be realized first"
  |MLtuple l ->
    assert (List.is_empty args);
    mkblock 0 (List.map (do_expr env []) l)

let do_Dfix rv c =
  let names = Array.to_list (Array.map id_of_global rv) in
  Array.iter
    (fun r -> if Table.is_custom r then failwith "Custom : unsupported") rv;
  (* Normally, no hack (MLexn "UNUSED") here, since no custom extraction *)
  let terms = Array.to_list (Array.map (do_expr [] []) c) in
  names, List.combine names terms

let rec do_elems names cont = function
  |[] -> cont names
  |(Dind _ | Dtype _) :: elems -> do_elems names cont elems
  |Dterm (r,t,_) :: elems ->
    let id = id_of_global r in
    let e = do_expr [] [] t in
    let rest = do_elems (id::names) cont elems in
    Llet (Alias, id, e, rest)
  |Dfix (rv,c,_) :: elems ->
    let ids, defs = do_Dfix rv c in
    let rest = do_elems (List.rev_append ids names) cont elems in
    Lletrec (defs, rest)

(** Build a lambda expression aimed at creating a .cmo or .cmx
    It is made of a block with all definitions of the structure. *)

let lambda_for_compunit (s:ml_decl list) =
  reset_dummy ();
  let cont names = mkblock 0 (List.rev_map (fun id -> Lvar id) names) in
  let t = do_elems [] cont s in
  add_dummy_def t

(** Build a lambda expression aimed at being directly
    loaded in the toplevel (or dynlinked in the native coqtop).
    The "main" code might be given separately, if not we use
    the last declaration of the structure. *)

let lambda_for_eval (s:ml_decl list) (ot:ml_ast option) =
  reset_dummy ();
  let cont names =
    match ot with
    | None -> Lvar (List.hd names) (* no final code, we pick the last one *)
    | Some t -> do_expr [] [] t
  in
  let t = do_elems [] cont s in
  add_dummy_def t


(** Reconstruction of a Coq value from the computation result *)

type reconstruction_failure =
| FunctionalValue
| ProofOrTypeValue (* unused, we now produce an evar instead *)
| MlUntypableValue

exception CannotReconstruct of reconstruction_failure

let mk_hole =
  Glob_term.GHole
    (Loc.ghost, Evar_kinds.InternalHole, Misctypes.IntroAnonymous, None)

let mk_construct cons args =
  let head = Glob_term.GRef (Loc.ghost, ConstructRef cons, None) in
  Glob_term.GApp (Loc.ghost, head, args)

let rec reconstruct typ o = match typ with
  |Tarr _ -> raise (CannotReconstruct FunctionalValue)
  |Tdummy _ -> mk_hole (*raise (CannotReconstruct ProofOrTypeValue)*)
  |Tunknown -> raise (CannotReconstruct MlUntypableValue)
  |Tmeta {contents = Some typ' } -> reconstruct typ' o
  |Tmeta _ | Tvar _ | Tvar' _ | Taxiom -> assert false
  |Tglob (r,typ_args) ->
    match r with
    |ConstRef cst ->
      let cb = Global.lookup_constant cst in
      (match Table.lookup_typedef cst cb with
       | None -> assert false
       | Some typ ->
          reconstruct (Mlutil.type_subst_list typ_args typ) o)
    |IndRef ind -> reconstruct_ind ind typ_args o
    |_ -> assert false

and reconstruct_ind ((kn,i) as ind) typ_args o =
  if Table.is_coinductive (IndRef ind)
  then failwith "Cannot print coinductives";
  (* unlike extract_inductive, the types in lookup_ind still
     includes Tdummy :-) *)
  let ml_ind =
    let mib = Global.lookup_mind kn in
    match Table.lookup_ind kn mib with
    | None -> assert false
    | Some mlind -> mlind.ind_packets.(i)
  in
  let info = get_ind_info ind in
  let cons, args =
    if Obj.is_block o then
      let n = Obj.tag o in
      assert (n < Array.length info.ind_nonconsts);
      info.ind_nonconsts.(n), (Obj.obj o : Obj.t array)
    else
      let n = Obj.obj o in
      assert (n < Array.length info.ind_consts);
      info.ind_consts.(n), [||]
  in
  assert (List.length typ_args = List.length ml_ind.ip_vars);
  let typs = Array.of_list ml_ind.ip_types.(snd cons - 1) in
  let typs' = Array.map (Mlutil.type_subst_list typ_args) typs in
  let nparams = List.length ml_ind.ip_sign in
  mk_construct cons (reconstruct_ind_args nparams typs' args)

(** Insert holes at the right place in constructor arguments *)

and reconstruct_ind_args nparams typs args =
  let size = nparams + Array.length typs in
  let res = Array.make size mk_hole in
  let a = ref (Array.to_list args) in
  let next_arg () = match !a with [] -> assert false | t::l -> a:=l; t in
  let no_more_arg () = List.is_empty !a in
  Array.iteri (fun i ty ->
    if not (Mlutil.isTdummy ty) then
      res.(nparams+i) <- reconstruct ty (next_arg ())) typs;
  assert (no_more_arg ());
  Array.to_list res

let cannot_reconstruct_msg = function
| FunctionalValue -> "function encountered"
| ProofOrTypeValue -> "proof part or type encountered"
| MlUntypableValue -> "ML untypable term encountered"


(** Utility functions concerning [ml_struct] *)

let flatten_structure s =
  List.map snd (List.flatten (List.map snd s))

let get_struct q =
  let r = ConstRef (Nametab.locate_constant q) in
  let s =
    Modutil.optimize_struct ([r],[]) (Extract_env.mono_environment [r] [])
  in flatten_structure s

(* TODO:
   X MLexn ...
   X MLdummy as __ rather than ()
   X Better extraction of MLcase when mere projection ? Really useful ? NO
   X Coinductives
   X Modules
   X Tests : rec mutuels, modules, records, avec dummy, ...
   X Reconstruction of constr when possible
   X Or rather to glob_constr with retyping (e.g. for lists)
   - Extraction to native code ...
   X Avoid the need to define a temp constant as "main"
   X Disable the Obj.magic production (no need to type-check :-)

   - get_db_name and __ ??

   Beware: in extraction of Coq's bool, true comes first, hence mapped to
   OCaml's 0 = false !!!
*)
