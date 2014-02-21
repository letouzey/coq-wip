(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*s From MiniML to ocaml internal Lambda AST *)

open Pp
open Err
open Util
open Names
open Nameops
open Globnames
open Table
open Miniml
open Mlutil
open Modutil
open Common

open Lambda (* in compiler-libs *)

let mkint n = Lconst (Const_base (Asttypes.Const_int n))
let mkchar c = Lconst (Const_base (Asttypes.Const_char c))
let mkblock tag args = Lprim (Pmakeblock (tag,Asttypes.Immutable), args)

let id_of_id id = Ident.create (Id.to_string id)
let id_of_mlid id = id_of_id (Mlutil.id_of_mlid id)

(** Table of global names *)

let global_table = (Hashtbl.create 47 : (global_reference, Ident.t) Hashtbl.t)

let id_of_global r =
  if is_inline_custom r then failwith "Custom : unsupported";
  try Hashtbl.find global_table r
  with Not_found ->
    assert (isConstRef r);
    let id = Ident.create (Constant.to_string (destConstRef r)) in
    Hashtbl.add global_table r id;
    id

(** Table of inductives and constructors *)

type ind_info =
  { ind_tags : Types.constructor_tag array;
    ind_consts : int; (* number of constant constructors *)
    ind_nonconsts : int } (* number of non-constant constructors *)

let ind_table = (Hashtbl.create 47 : (inductive, ind_info) Hashtbl.t)

let get_mlind (kn,i) =
  try (snd (Table.lookup_ind kn)).ind_packets.(i)
  with _ -> assert false

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
    let info = {
      ind_tags = tags;
      ind_consts = !nb_const;
      ind_nonconsts = !nb_block }
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
           Types.cstr_consts = info.ind_consts;
           Types.cstr_nonconsts = info.ind_nonconsts;
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
    let fl,a' = collect_lams a in
    let fl = List.map id_of_mlid fl in
    let env' = push_vars fl env in
    let st = Lfunction (Curried, List.rev fl, do_expr env' [] a') in
    apply st args
  |MLletin (id,a1,a2) ->
    let id' = id_of_mlid id in
    let env' = push_vars [id'] env in
    let e1 = do_expr env [] a1 in
    let e2 = do_expr env' [] a2 in
    apply (Llet (Alias, id', e1, e2)) args
    (* TODO: Alias is only true for pure extr*)
  |MLglob r ->
    (* TODO : handle records as getfield...
    (try
       let args = List.skipn (projection_arity r) args in
       let record = List.hd args in
       pp_apply (record ++ str "." ++ pp_global Term r) par (List.tl args)
     with e when Err.noncritical e ->
    *)
    apply (Lvar (id_of_global r)) args
  |MLcons (_,r,a) as c ->
    assert (List.is_empty args);
    begin match a with
    | _ when is_native_char c -> mkchar (get_native_char c)
    | _ when is_coinductive r -> failwith "coinductive unsupported"
    | [] ->
      (match get_cons_tag r with
      | Types.Cstr_constant n -> mkint n
      | _ -> assert false)
    | _ ->
      (* TODO : support records ...
      let fds = get_record_fields r in
      if not (List.is_empty fds) then
	pp_record_pat (pp_fields r fds, List.map (pp_expr true env []) a)
      else *)
      let a' = List.map (do_expr env []) a in
      match get_cons_tag r with
      | Types.Cstr_block tag -> mkblock tag a'
      | _ -> assert false
    end
  |MLcase (typ, t, pv) ->
    if is_custom_match pv then failwith "unsupported custom match";
    if is_coinductive_type typ then failwith "unsupported coinductive";
    let head = do_expr env [] t in
    (* TODO: handle the match that are mere record projection ... *)
    let branches = List.map (do_one_pat env) (Array.to_list pv) in
    Matching.for_function Location.none None head branches Typedtree.Total
  |MLfix (i,ids,defs) ->
    let rev_ids = List.rev_map id_of_id (Array.to_list ids) in
    let env' = push_vars rev_ids env in
    let ids' = List.rev rev_ids in
    Lletrec
      (List.map2 (fun id t -> id, do_expr env' [] t) ids' (Array.to_list defs),
       apply (Lvar (List.nth ids' i)) args)
  |MLexn s -> failwith "MLexn TODO"
  |MLdummy -> mkint 0 (* TODO put someday the real __ *)
  |MLmagic a -> do_expr env args a
  |MLaxiom -> failwith "An axiom must be realized first"
  |MLtuple l ->
    assert (List.is_empty args);
    mkblock 0 (List.map (do_expr env []) l)

and do_one_pat env (ids,p,t) =
  let env' = push_vars (List.rev_map id_of_mlid ids) env in
  let p' = do_pattern env' p in
  let e = do_expr env' [] t in
  (p',e)

let do_Dfix rv c =
  let names = Array.to_list (Array.map id_of_global rv) in
  Array.iter (fun r -> if is_custom r then failwith "Custom : unsupported") rv;
  (* Normally, no hack (MLexn "UNUSED") here, since no custom extraction *)
  let terms = Array.to_list (Array.map (do_expr [] []) c) in
  names, List.combine names terms

let rec do_elems names cont = function
  |[] -> cont names
  |(_,(SEmodule _ | SEmodtype _)) :: _ -> failwith "unsupported inner modules"
  |(_,SEdecl (Dind _ | Dtype _)) :: elems -> do_elems names cont elems
  |(_,SEdecl (Dterm (r,t,_))) :: elems ->
    let id = id_of_global r in
    Llet (Alias, id, do_expr [] [] t, do_elems (id::names) cont elems)
  |(_,SEdecl (Dfix (rv,c,_))) :: elems ->
    let ids, defs = do_Dfix rv c in
    Lletrec (defs, do_elems (List.rev_append ids names) cont elems)

let do_structure_mod s =
  let cont names = mkblock 0 (List.rev_map (fun id -> Lvar id) names) in
  do_elems [] cont (List.flatten (List.map snd s))

let do_structure_phrase s =
  let cont names = Lvar (List.hd names) in
  do_elems [] cont (List.flatten (List.map snd s))

let debug_struct q =
  let r = ConstRef (Nametab.locate_constant q) in
  Modutil.optimize_struct ([r],[]) (Extract_env.mono_environment [r] [])

let debug_all q =
  Printlambda.lambda Format.std_formatter (do_structure_mod (debug_struct q))

let make_cmo modulename (structure:ml_structure) =
  reset_tables ();
  Translobj.reset_labels ();
  Translmod.primitive_declarations := [];
  Env.reset_cache ();
  let mod_id = Ident.create_persistent modulename in
  let lambda = Lprim (Psetglobal mod_id,[do_structure_mod structure]) in
  (* borrowed from drivers/compile.ml *)
  let lambda' = Simplif.simplify_lambda lambda in
  Printlambda.lambda Format.std_formatter lambda';
  let bytecode = Bytegen.compile_implementation modulename lambda' in
  Printinstr.instrlist Format.std_formatter bytecode;
  let oc = open_out_bin (modulename^".cmo") in
  let () = Emitcode.to_file oc modulename bytecode in
  close_out oc

let direct_eval (structure:ml_structure) =
  let lam = do_structure_phrase structure in
  (* borrowed from toplevel/toploop.ml *)
  let slam = Simplif.simplify_lambda lam in
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  try
    let retval = (Meta.reify_bytecode code code_size) () in
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    retval
  with x ->
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Symtable.restore_state initial_symtable;
    raise x
