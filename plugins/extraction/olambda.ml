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

open Lambda (* in compiler-libs *)

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

(** Table of global names *)

let global_table = (Hashtbl.create 47 : (global_reference, Ident.t) Hashtbl.t)

let id_of_global r =
  if Table.is_inline_custom r then failwith "Custom : unsupported";
  try Hashtbl.find global_table r
  with Not_found ->
    assert (isConstRef r);
    let id = Ident.create (Constant.to_string (destConstRef r)) in
    Hashtbl.add global_table r id;
    id

(** Table of inductives and constructors *)

type ind_info =
  { ind_tags : Types.constructor_tag array;
    ind_consts : constructor array; (* constant constructors, by index *)
    ind_nonconsts : constructor array } (* non-csts constructors, by tags *)

let ind_table = (Hashtbl.create 47 : (inductive, ind_info) Hashtbl.t)

let get_mlind (kn,i) =
  let mi = Extraction.extract_inductive (Global.env ()) kn in
  mi.ind_packets.(i)

let get_ind_info ind =
  try Hashtbl.find ind_table ind
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
    Hashtbl.add ind_table ind info;
    info

let cons_table = (Hashtbl.create 47 :
                    (constructor, Types.constructor_description) Hashtbl.t)

let get_cons_tag = function
  |ConstructRef (ind,j) -> (get_ind_info ind).ind_tags.(j-1)
  |_ -> assert false

let get_cons_desc = function
  |ConstructRef (ind,j) ->
    (try Hashtbl.find cons_table (ind,j)
     with Not_found ->
       let mlind = get_mlind ind in
       let info = get_ind_info ind in
       let desc =
         { Types.cstr_name = Id.to_string (mlind.ip_consnames.(j-1));
           Types.cstr_arity = List.length (mlind.ip_types.(j-1));
           Types.cstr_tag = info.ind_tags.(j-1);
           Types.cstr_consts = Array.length info.ind_consts;
           Types.cstr_nonconsts = Array.length info.ind_nonconsts;
           (* All other fields aren't used by Matching.ml : *)
           Types.cstr_res = Ctype.none;
           Types.cstr_existentials = [];
           Types.cstr_args = [];
           Types.cstr_normal = 0;
           Types.cstr_generalized = true;
           Types.cstr_private = Asttypes.Public }
       in
       Hashtbl.add cons_table (ind,j) desc;
       desc)
  |_ -> assert false

let reset_tables () =
  Hashtbl.clear global_table;
  Hashtbl.clear ind_table;
  Hashtbl.clear cons_table

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

(** Patterns *)

let rec gen_usual_args n = (* Prel n; ... ; Prel 1 *)
  if n = 0 then [] else Prel n :: (gen_usual_args (n-1))

let rec do_pattern env pat = (* fake pattern with just enough for Matching.ml *)
  { Parmatch.omega with Typedtree.pat_desc = do_pat_desc env pat }

and do_pat_desc env = function
  |Pwild -> Typedtree.Tpat_any
  |Prel n ->
    let id = get_db_name n env in
    Typedtree.Tpat_var (id,Location.mknoloc (Ident.name id))
  |Ptuple l -> Typedtree.Tpat_tuple (List.map (do_pattern env) l)
  |Pcons (r,l) ->
    let desc = get_cons_desc r in
    let args = List.map (do_pattern env) l in
    let consname = Location.mknoloc (Longident.Lident desc.Types.cstr_name) in
    Typedtree.Tpat_construct (consname,desc,args,false)
  |Pusual r ->
    let desc = get_cons_desc r in
    let arity = desc.Types.cstr_arity in
    let args = List.map (do_pattern env) (gen_usual_args arity) in
    let consname = Location.mknoloc (Longident.Lident desc.Types.cstr_name) in
    Typedtree.Tpat_construct (consname,desc,args,false)

(** Terms *)

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
    let head = do_expr env [] t in
    let head = if Table.is_coinductive_type typ then mkforce head else head in
    (* NB: the matching compilation below will optimize the matchs
       that are mere record projections. *)
    let do_one_pat (ids,p,t) =
      let env' = push_vars (List.rev_map id_of_mlid ids) env in
      (do_pattern env' p, do_expr env' [] t)
    in
    let branches = List.map do_one_pat (Array.to_list pv) in
    Matching.for_function Location.none None head branches Typedtree.Total
  |MLfix (i,ids,defs) ->
    let rev_ids = List.rev_map id_of_id (Array.to_list ids) in
    let env' = push_vars rev_ids env in
    let ids' = List.rev rev_ids in
    Lletrec
      (List.map2 (fun id t -> id, do_expr env' [] t) ids' (Array.to_list defs),
       apply (Lvar (List.nth ids' i)) args)
  |MLexn s -> Lprim (Praise, [mkblock 0 [mkexn s]])
  |MLdummy -> mkint 0 (* TODO put someday the real __ *)
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
  |(SEmodule _ | SEmodtype _) :: _ -> failwith "unsupported inner modules"
  |SEdecl (Dind _ | Dtype _) :: elems -> do_elems names cont elems
  |SEdecl (Dterm (r,t,_)) :: elems ->
    let id = id_of_global r in
    let e = do_expr [] [] t in
    let rest = do_elems (id::names) cont elems in
    Llet (Alias, id, e, rest)
  |SEdecl (Dfix (rv,c,_)) :: elems ->
    let ids, defs = do_Dfix rv c in
    let rest = do_elems (List.rev_append ids names) cont elems in
    Lletrec (defs, rest)

(** Build a lambda expression aimed at creating a .cmo or .cmx
    It is made of a block with all toplevel definitions
    of the structure. *)

let lambda_for_compunit (s:ml_flat_structure) =
  let cont names = mkblock 0 (List.rev_map (fun id -> Lvar id) names) in
  do_elems [] cont s

(** Build a lambda expression aimed at behind directly
    loaded in the toplevel (or dynlinked in the native coqtop).
    The "main" code might be given separately, if not we use
    the last declaration of the structure. *)

let lambda_for_eval (s:ml_flat_structure) (ot:ml_ast option) =
  let cont names =
    match ot with
    | None -> Lvar (List.hd names) (* no final code, we pick the last one *)
    | Some t -> do_expr [] [] t
  in
  do_elems [] cont s



type reconstruction_failure =
| FunctionalValue
| ProofOrTypeValue
| MlUntypableValue

exception CannotReconstruct of reconstruction_failure

let rec reconstruct typ o = match typ with
  |Tarr _ -> raise (CannotReconstruct FunctionalValue)
  |Tdummy _ -> raise (CannotReconstruct ProofOrTypeValue)
  |Tunknown -> raise (CannotReconstruct MlUntypableValue)
  |Tmeta {contents = Some typ' } -> reconstruct typ' o
  |Tmeta _ | Tvar _ | Tvar' _ | Taxiom -> assert false
  |Tglob (r,typ_args) ->
    match r with
    |ConstRef cst ->
      let arity,typ = Table.lookup_type cst in
      assert (List.length typ_args = arity); (* TODO : is this sure ? *)
      reconstruct (Mlutil.type_subst_list typ_args typ) o
    |IndRef ind -> reconstruct_ind ind typ_args o
    |_ -> assert false

and reconstruct_ind ((kn,i) as ind) typ_args o =
  if Table.is_coinductive (IndRef ind)
  then failwith "Cannot print coinductives";
  (* unlike extract_inductive, the types in lookup_ind still
     includes Tdummy :-) *)
  let ml_ind =
    try (snd (Table.lookup_ind kn)).ind_packets.(i)
    with _ -> assert false
  in
  let info = get_ind_info ind in
  let cons, args =
    if Obj.is_block o then
      info.ind_nonconsts.(Obj.tag o), (Obj.obj o : Obj.t array)
    else
      info.ind_consts.(Obj.obj o), [||]
  in
  let typs = Array.of_list ml_ind.ip_types.(snd cons - 1) in
  if Array.length typs <> Array.length args
  then raise (CannotReconstruct ProofOrTypeValue);
  assert (List.length typ_args = List.length ml_ind.ip_vars);
  let typs' = Array.map (Mlutil.type_subst_list typ_args) typs in
  let args = Array.map2 reconstruct typs' args in
  Term.mkApp (Term.mkConstruct cons, args)

let make_cmo ?(debug=false) modulename (structure:ml_flat_structure) =
  Olambda_byte.reset_compiler ();
  Olambda_byte.compile_lambda ~debug modulename
    (lambda_for_compunit structure)

let cannot_reconstruct_msg = function
| FunctionalValue -> "function encountered"
| ProofOrTypeValue -> "proof part or type encountered"
| MlUntypableValue -> "ML untypable term encountered"

let flatten_structure s =
  List.map snd (List.flatten (List.map snd s))

let get_struct q =
  let r = ConstRef (Nametab.locate_constant q) in
  let s =
    Modutil.optimize_struct ([r],[]) (Extract_env.mono_environment [r] [])
  in flatten_structure s

let direct_eval ?(debug=false) (s:ml_flat_structure) ot =
  Olambda_byte.eval_lambda ~debug (lambda_for_eval s ot)

let extraction_compute c =
  try
    let s,t,ty = Extract_env.structure_for_compute c in
    reconstruct ty (direct_eval s (Some t))
  with CannotReconstruct r ->
    Errors.error ("Cannot reconstruct a Coq value : " ^
                  cannot_reconstruct_msg r)

(* TODO:
   X MLexn ...
   - MLdummy as __ rather than ()
   X Better extraction of MLcase when mere projection ? Really useful ? NO
   X Coinductives
   - Tests : rec mutuels, modules, records, avec dummy, ...
   X Reconstruction of constr when possible
   - Or rather to glob_constr with retyping (e.g. for lists)
     Cf. Pretyping.understand ...
   - Extraction to native code ...
   X Avoid the need to define a temp constant as "main"
   - Disable the Obj.magic production (no need to type-check :-)

   Beware: in extraction of Coq's bool, true comes first, hence mapped to
   OCaml's 0 = false !!!
*)
