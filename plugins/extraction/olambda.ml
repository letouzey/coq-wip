(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*s Production of Ocaml syntax. *)

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

(* in compiler-libs *)
open Lambda

(*s Some utility functions. *)

let pp_abst = function
  | [] -> mt ()
  | l  ->
      str "fun " ++ prlist_with_sep (fun () -> str " ") pr_id l ++
      str " ->" ++ spc ()

let pp_letin pat def body =
  let fstline = str "let " ++ pat ++ str " =" ++ spc () ++ def in
  hv 0 (hv 0 (hov 2 fstline ++ spc () ++ str "in") ++ spc () ++ hov 0 body)

(*s From MiniML to ocaml internal Lambda *)

let global_table = Hashtbl.create 47

let id_of_global r =
  if is_inline_custom r then failwith "Inline unsupported";
  try Hashtbl.find global_table r
  with Not_found ->
    assert (isConstRef r);
    let id = Ident.create (Constant.to_string (destConstRef r)) in
    Hashtbl.add global_table r id;
    id

let cons_table = Hashtbl.create 47

let do_cons r =
  match r with
  | ConstructRef (ind,j) ->
    (try (Hashtbl.find cons_table ind).(j-1)
     with Not_found ->
       let (kn,i) = ind in
       let mlind =
         try (snd (Table.lookup_ind kn)).ind_packets.(i)
         with _ -> assert false
       in
       let nb_atom = ref (-1) and nb_block = ref (-1) in
       let ans = Array.map
         (function
         | [] -> incr nb_atom; Types.Cstr_constant (!nb_atom)
         | _ -> incr nb_block; Types.Cstr_block (!nb_block))
         mlind.ip_types
       in
       Hashtbl.add cons_table ind ans;
       ans.(j-1))
  | _ -> assert false

let mkint n = Lconst (Const_base (Asttypes.Const_int n))
let mkchar c = Lconst (Const_base (Asttypes.Const_char c))

let get_db_name n db =
  let id = List.nth db (pred n) in
  id
  (* TODO if Id.equal id dummy_name then Id.of_string "__" else id *)

let push_vars ids db = ids @ db

let id_of_id id = Ident.create (Id.to_string id)
let id_of_mlid id = id_of_id (Mlutil.id_of_mlid id)

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
      (match do_cons r with
      | Types.Cstr_constant n -> mkint n
      | _ -> assert false)
    | _ ->
      (* TODO : support records ...
      let fds = get_record_fields r in
      if not (List.is_empty fds) then
	pp_record_pat (pp_fields r fds, List.map (pp_expr true env []) a)
      else *)
      let a' = List.map (do_expr env []) a in
      match do_cons r with
      | Types.Cstr_block tag -> Lprim (Pmakeblock (tag,Asttypes.Immutable),a')
      | _ -> assert false
    end
  |MLcase (typ, t, pv) ->
    if is_custom_match pv then failwith "unsupported custom match";
    if is_coinductive_type typ then failwith "unsupported coinductive";
    let head = do_expr env [] t in
    (* TODO: handle the match that are mere record projection ... *)
    (*
    apply
      (str "match " ++ head ++ str " with" ++ fnl () ++
	      pp_pat env pv)

      Matching.for_function Location.none None
        ...
        Typedtree.Total
    *)
    failwith "TODO"

  |MLfix (i,ids,defs) ->
    let ids' = List.rev_map id_of_id (Array.to_list ids) in
    let env' = push_vars ids' env in
    do_fix env' i (Array.of_list (List.rev ids'),defs) args
  |MLexn s -> failwith "MLexn TODO"
  |MLdummy -> mkint 0 (* TODO put someday the real __ *)
  |MLmagic a -> do_expr env args a
  |MLaxiom -> failwith "MLaxiom unsupported"
  |MLtuple _ -> failwith "MLtuple unsupported"

and do_fix env e args = failwith "TODO"

(*
  |MLcase (_, t, pv) when is_custom_match pv ->
    if not (is_regular_match pv) then
      error "Cannot mix yet user-given match and general patterns.";
    let mkfun (ids,_,e) =
      if not (List.is_empty ids) then named_lams (List.rev ids) e
      else dummy_lams (ast_lift 1 e) 1
    in
    let pp_branch tr = pp_expr true env [] (mkfun tr) ++ fnl () in
    let inner =
      str (find_custom_match pv) ++ fnl () ++
	prvect pp_branch pv ++
	pp_expr true env [] t
    in
    apply2 (hov 2 inner)
  |MLcase (typ, t, pv) ->
    let head =
      if not (is_coinductive_type typ) then pp_expr false env [] t
      else (str "Lazy.force" ++ spc () ++ pp_expr true env [] t)
    in
    (* First, can this match be printed as a mere record projection ? *)
    (try pp_record_proj par env typ t pv args
     with Impossible ->
       (* Second, can this match be printed as a let-in ? *)
       if Int.equal (Array.length pv) 1 then
	 let s1,s2 = pp_one_pat env pv.(0) in
	 hv 0 (apply2 (pp_letin s1 head s2))
       else
	 (* Third, can this match be printed as [if ... then ... else] ? *)
	 (try apply2 (pp_ifthenelse env head pv)
	  with Not_found ->
	    (* Otherwise, standard match *)
	    apply2
	      (v 0 (str "match " ++ head ++ str " with" ++ fnl () ++
		      pp_pat env pv))))
*)


(*
and pp_record_proj par env typ t pv args =
  (* Can a match be printed as a mere record projection ? *)
  let fields = record_fields_of_type typ in
  if List.is_empty fields then raise Impossible;
  if not (Int.equal (Array.length pv) 1) then raise Impossible;
  if has_deep_pattern pv then raise Impossible;
  let (ids,pat,body) = pv.(0) in
  let n = List.length ids in
  let no_patvar a = not (List.exists (ast_occurs_itvl 1 n) a) in
  let rel_i,a = match body with
    | MLrel i when i <= n -> i,[]
    | MLapp(MLrel i, a) when i<=n && no_patvar a -> i,a
    | _ -> raise Impossible
  in
  let rec lookup_rel i idx = function
    | Prel j :: l -> if Int.equal i j then idx else lookup_rel i (idx+1) l
    | Pwild :: l -> lookup_rel i (idx+1) l
    | _ -> raise Impossible
  in
  let r,idx = match pat with
    | Pusual r -> r, n-rel_i
    | Pcons (r,l) -> r, lookup_rel rel_i 0 l
    | _ -> raise Impossible
  in
  if is_infix r then raise Impossible;
  let env' = snd (push_vars (List.rev_map id_of_mlid ids) env) in
  let pp_args = (List.map (pp_expr true env' []) a) @ args in
  let pp_head = pp_expr true env [] t ++ str "." ++ pp_field r fields idx
  in
  pp_apply pp_head par pp_args

and pp_record_pat (fields, args) =
   str "{ " ++
   prlist_with_sep (fun () -> str ";" ++ spc ())
     (fun (f,a) -> f ++ str " =" ++ spc () ++ a)
     (List.combine fields args) ++
   str " }"

and pp_cons_pat r ppl =
  if is_infix r && Int.equal (List.length ppl) 2 then
    List.hd ppl ++ str (get_infix r) ++ List.hd (List.tl ppl)
  else
    let fields = get_record_fields r in
    if not (List.is_empty fields) then pp_record_pat (pp_fields r fields, ppl)
    else if String.is_empty (str_global Cons r) then
      pp_boxed_tuple identity ppl (* Hack Extract Inductive prod *)
    else
      pp_global Cons r ++ space_if (not (List.is_empty ppl)) ++ pp_boxed_tuple identity ppl

and pp_gen_pat ids env = function
  | Pcons (r, l) -> pp_cons_pat r (List.map (pp_gen_pat ids env) l)
  | Pusual r -> pp_cons_pat r (List.map pr_id ids)
  | Ptuple l -> pp_boxed_tuple (pp_gen_pat ids env) l
  | Pwild -> str "_"
  | Prel n -> pr_id (get_db_name n env)

and pp_ifthenelse env expr pv = match pv with
  | [|([],tru,the);([],fal,els)|] when
      (is_bool_patt tru "true") && (is_bool_patt fal "false")
      ->
      hv 0 (hov 2 (str "if " ++ expr) ++ spc () ++
            hov 2 (str "then " ++
		   hov 2 (pp_expr (expr_needs_par the) env [] the)) ++ spc () ++
	    hov 2 (str "else " ++
	           hov 2 (pp_expr (expr_needs_par els) env [] els)))
  | _ -> raise Not_found

and pp_one_pat env (ids,p,t) =
  let ids',env' = push_vars (List.rev_map id_of_mlid ids) env in
  pp_gen_pat (List.rev ids') env' p,
  pp_expr (expr_needs_par t) env' [] t

and pp_pat env pv =
  prvecti
    (fun i x ->
       let s1,s2 = pp_one_pat env x in
       hv 2 (hov 4 (str "| " ++ s1 ++ str " ->") ++ spc () ++ hov 2 s2) ++
       if Int.equal i (Array.length pv - 1) then mt () else fnl ())
    pv

and pp_function env t =
  let bl,t' = collect_lams t in
  let bl,env' = push_vars (List.map id_of_mlid bl) env in
  match t' with
    | MLcase(Tglob(r,_),MLrel 1,pv) when
	not (is_coinductive r) && List.is_empty (get_record_fields r) &&
	not (is_custom_match pv) ->
	if not (ast_occurs 1 (MLcase(Tunknown,MLdummy,pv))) then
	  pr_binding (List.rev (List.tl bl)) ++
	  str " = function" ++ fnl () ++
	  v 0 (pp_pat env' pv)
	else
          pr_binding (List.rev bl) ++
          str " = match " ++ pr_id (List.hd bl) ++ str " with" ++ fnl () ++
	  v 0 (pp_pat env' pv)
    | _ ->
          pr_binding (List.rev bl) ++
	  str " =" ++ fnl () ++ str "  " ++
	  hov 2 (pp_expr false env' [] t')

(*s names of the functions ([ids]) are already pushed in [env],
    and passed here just for convenience. *)

and pp_fix par env i (ids,bl) args =
  pp_par par
    (v 0 (str "let rec " ++
	  prvect_with_sep
	    (fun () -> fnl () ++ str "and ")
	    (fun (fi,ti) -> pr_id fi ++ pp_function env ti)
	    (Array.map2 (fun id b -> (id,b)) ids bl) ++
	  fnl () ++
	  hov 2 (str "in " ++ pp_apply (pr_id ids.(i)) false args)))

let pp_val e typ =
  hov 4 (str "(** val " ++ e ++ str " :" ++ spc () ++ pp_type false [] typ ++
  str " **)")  ++ fnl2 ()

(*s Pretty-printing of [Dfix] *)

let pp_Dfix (rv,c,t) =
  let names = Array.map
    (fun r -> if is_inline_custom r then mt () else pp_global Term r) rv
  in
  let rec pp init i =
    if i >= Array.length rv then
      (if init then failwith "empty phrase" else mt ())
    else
      let void = is_inline_custom rv.(i) ||
	(not (is_custom rv.(i)) && match c.(i) with MLexn "UNUSED" -> true | _ -> false)
      in
      if void then pp init (i+1)
      else
	let def =
	  if is_custom rv.(i) then str " = " ++ str (find_custom rv.(i))
	  else pp_function (empty_env ()) c.(i)
	in
	(if init then mt () else fnl2 ()) ++
	pp_val names.(i) t.(i) ++
	str (if init then "let rec " else "and ") ++ names.(i) ++ def ++
	pp false (i+1)
  in pp true 0

(*s Pretty-printing of inductive types declaration. *)

let pp_equiv param_list name = function
  | NoEquiv, _ -> mt ()
  | Equiv kn, i ->
      str " = " ++ pp_parameters param_list ++ pp_global Type (IndRef (mind_of_kn kn,i))
  | RenEquiv ren, _  ->
      str " = " ++ pp_parameters param_list ++ str (ren^".") ++ name


let pp_one_ind prefix ip_equiv pl name cnames ctyps =
  let pl = rename_tvars keywords pl in
  let pp_constructor i typs =
    (if Int.equal i 0 then mt () else fnl ()) ++
    hov 3 (str "| " ++ cnames.(i) ++
	   (if List.is_empty typs then mt () else str " of ") ++
	   prlist_with_sep
	    (fun () -> spc () ++ str "* ") (pp_type true pl) typs)
  in
  pp_parameters pl ++ str prefix ++ name ++
  pp_equiv pl name ip_equiv ++ str " =" ++
  if Int.equal (Array.length ctyps) 0 then str " unit (* empty inductive *)"
  else fnl () ++ v 0 (prvecti pp_constructor ctyps)

let pp_logical_ind packet =
  pp_comment (pr_id packet.ip_typename ++ str " : logical inductive") ++
  fnl () ++
  pp_comment (str "with constructors : " ++
	      prvect_with_sep spc pr_id packet.ip_consnames) ++
  fnl ()

let pp_singleton kn packet =
  let name = pp_global Type (IndRef (kn,0)) in
  let l = rename_tvars keywords packet.ip_vars in
  hov 2 (str "type " ++ pp_parameters l ++ name ++ str " =" ++ spc () ++
	 pp_type false l (List.hd packet.ip_types.(0)) ++ fnl () ++
	 pp_comment (str "singleton inductive, whose constructor was " ++
		     pr_id packet.ip_consnames.(0)))

let pp_record kn fields ip_equiv packet =
  let ind = IndRef (kn,0) in
  let name = pp_global Type ind in
  let fieldnames = pp_fields ind fields in
  let l = List.combine fieldnames packet.ip_types.(0) in
  let pl = rename_tvars keywords packet.ip_vars in
  str "type " ++ pp_parameters pl ++ name ++
  pp_equiv pl name ip_equiv ++ str " = { "++
  hov 0 (prlist_with_sep (fun () -> str ";" ++ spc ())
	   (fun (p,t) -> p ++ str " : " ++ pp_type true pl t) l)
  ++ str " }"

let pp_coind pl name =
  let pl = rename_tvars keywords pl in
  pp_parameters pl ++ name ++ str " = " ++
  pp_parameters pl ++ str "__" ++ name ++ str " Lazy.t" ++
  fnl() ++ str "and "

let pp_ind co kn ind =
  let prefix = if co then "__" else "" in
  let some = ref false in
  let init= ref (str "type ") in
  let names =
    Array.mapi (fun i p -> if p.ip_logical then mt () else
		  pp_global Type (IndRef (kn,i)))
      ind.ind_packets
  in
  let cnames =
    Array.mapi
      (fun i p -> if p.ip_logical then [||] else
	 Array.mapi (fun j _ -> pp_global Cons (ConstructRef ((kn,i),j+1)))
	   p.ip_types)
      ind.ind_packets
  in
  let rec pp i =
    if i >= Array.length ind.ind_packets then mt ()
    else
      let ip = (kn,i) in
      let ip_equiv = ind.ind_equiv, i in
      let p = ind.ind_packets.(i) in
      if is_custom (IndRef ip) then pp (i+1)
      else begin
	some := true;
	if p.ip_logical then pp_logical_ind p ++ pp (i+1)
	else
	  let s = !init in
	  begin
	    init := (fnl () ++ str "and ");
	    s ++
	    (if co then pp_coind p.ip_vars names.(i) else mt ()) ++
	    pp_one_ind
	      prefix ip_equiv p.ip_vars names.(i) cnames.(i) p.ip_types ++
	    pp (i+1)
	  end
      end
  in
  let st = pp 0 in if !some then st else failwith "empty phrase"


(*s Pretty-printing of a declaration. *)

let pp_mind kn i =
  match i.ind_kind with
    | Singleton -> pp_singleton kn i.ind_packets.(0)
    | Coinductive -> pp_ind true kn i
    | Record fields -> pp_record kn fields (i.ind_equiv,0) i.ind_packets.(0)
    | Standard -> pp_ind false kn i

let pp_decl = function
    | Dtype (r,_,_) when is_inline_custom r -> failwith "empty phrase"
    | Dterm (r,_,_) when is_inline_custom r -> failwith "empty phrase"
    | Dind (kn,i) -> pp_mind kn i
    | Dtype (r, l, t) ->
        let name = pp_global Type r in
	let l = rename_tvars keywords l in
        let ids, def =
	  try
	    let ids,s = find_type_custom r in
	    pp_string_parameters ids, str "=" ++ spc () ++ str s
	  with Not_found ->
	    pp_parameters l,
	    if t == Taxiom then str "(* AXIOM TO BE REALIZED *)"
	    else str "=" ++ spc () ++ pp_type false l t
	in
	hov 2 (str "type " ++ ids ++ name ++ spc () ++ def)
    | Dterm (r, a, t) ->
	let def =
	  if is_custom r then str (" = " ^ find_custom r)
	  else if is_projection r then
	    (prvect str (Array.make (projection_arity r) " _")) ++
	    str " x = x."
	  else pp_function (empty_env ()) a
	in
	let name = pp_global Term r in
	let postdef = if is_projection r then name else mt () in
	pp_val name t ++ hov 0 (str "let " ++ name ++ def ++ postdef)
    | Dfix (rv,defs,typs) ->
	pp_Dfix (rv,defs,typs)

let pp_alias_decl ren = function
  | Dind (kn,i) -> pp_mind kn { i with ind_equiv = RenEquiv ren }
  | Dtype (r, l, _) ->
      let name = pp_global Type r in
      let l = rename_tvars keywords l in
      let ids = pp_parameters l in
      hov 2 (str "type " ++ ids ++ name ++ str " =" ++ spc () ++ ids ++
	     str (ren^".") ++ name)
  | Dterm (r, a, t) ->
      let name = pp_global Term r in
      hov 2 (str "let " ++ name ++ str (" = "^ren^".") ++ name)
  | Dfix (rv, _, _) ->
      prvecti (fun i r -> if is_inline_custom r then mt () else
		 let name = pp_global Term r in
		 hov 2 (str "let " ++ name ++ str (" = "^ren^".") ++ name) ++
		 fnl ())
	rv

let pp_spec = function
  | Sval (r,_) when is_inline_custom r -> failwith "empty phrase"
  | Stype (r,_,_) when is_inline_custom r -> failwith "empty phrase"
  | Sind (kn,i) -> pp_mind kn i
  | Sval (r,t) ->
      let def = pp_type false [] t in
      let name = pp_global Term r in
      hov 2 (str "val " ++ name ++ str " :" ++ spc () ++ def)
  | Stype (r,vl,ot) ->
      let name = pp_global Type r in
      let l = rename_tvars keywords vl in
      let ids, def =
	try
	  let ids, s = find_type_custom r in
	  pp_string_parameters ids,  str "= " ++ str s
	with Not_found ->
	  let ids = pp_parameters l in
	  match ot with
	    | None -> ids, mt ()
	    | Some Taxiom -> ids, str "(* AXIOM TO BE REALIZED *)"
	    | Some t -> ids, str "=" ++ spc () ++ pp_type false l t
      in
      hov 2 (str "type " ++ ids ++ name ++ spc () ++ def)

let pp_alias_spec ren = function
  | Sind (kn,i) -> pp_mind kn { i with ind_equiv = RenEquiv ren }
  | Stype (r,l,_) ->
      let name = pp_global Type r in
      let l = rename_tvars keywords l in
      let ids = pp_parameters l in
      hov 2 (str "type " ++ ids ++ name ++ str " =" ++ spc () ++ ids ++
	     str (ren^".") ++ name)
  | Sval _ -> assert false

let rec pp_specif = function
  | (_,Spec (Sval _ as s)) -> pp_spec s
  | (l,Spec s) ->
      (try
	 let ren = Common.check_duplicate (top_visible_mp ()) l in
	 hov 1 (str ("module "^ren^" : sig ") ++ fnl () ++ pp_spec s) ++
	 fnl () ++ str "end" ++ fnl () ++
	 pp_alias_spec ren s
       with Not_found -> pp_spec s)
  | (l,Smodule mt) ->
      let def = pp_module_type [] mt in
      let def' = pp_module_type [] mt in
      let name = pp_modname (MPdot (top_visible_mp (), l)) in
      hov 1 (str "module " ++ name ++ str " : " ++ fnl () ++ def) ++
      (try
	 let ren = Common.check_duplicate (top_visible_mp ()) l in
	 fnl () ++ hov 1 (str ("module "^ren^" : ") ++ fnl () ++ def')
       with Not_found -> Pp.mt ())
  | (l,Smodtype mt) ->
      let def = pp_module_type [] mt in
      let name = pp_modname (MPdot (top_visible_mp (), l)) in
      hov 1 (str "module type " ++ name ++ str " = " ++ fnl () ++ def) ++
      (try
	 let ren = Common.check_duplicate (top_visible_mp ()) l in
	 fnl () ++ str ("module type "^ren^" = ") ++ name
       with Not_found -> Pp.mt ())

and pp_module_type params = function
  | MTident kn ->
      pp_modname kn
  | MTfunsig (mbid, mt, mt') ->
      let typ = pp_module_type [] mt in
      let name = pp_modname (MPbound mbid) in
      let def = pp_module_type (MPbound mbid :: params) mt' in
      str "functor (" ++ name ++ str ":" ++ typ ++ str ") ->" ++ fnl () ++ def
  | MTsig (mp, sign) ->
      push_visible mp params;
      let l = List.map pp_specif sign in
      pop_visible ();
      str "sig " ++ fnl () ++
      v 1 (str " " ++ prlist_with_sep fnl2 identity l) ++
	fnl () ++ str "end"
  | MTwith(mt,ML_With_type(idl,vl,typ)) ->
      let ids = pp_parameters (rename_tvars keywords vl) in
      let mp_mt = msid_of_mt mt in
      let l,idl' = List.sep_last idl in
      let mp_w =
	List.fold_left (fun mp l -> MPdot(mp,Label.of_id l)) mp_mt idl'
      in
      let r = ConstRef (Constant.make2 mp_w (Label.of_id l)) in
      push_visible mp_mt [];
      let pp_w = str " with type " ++ ids ++ pp_global Type r in
      pop_visible();
      pp_module_type [] mt ++ pp_w ++ str " = " ++ pp_type false vl typ
  | MTwith(mt,ML_With_module(idl,mp)) ->
      let mp_mt = msid_of_mt mt in
      let mp_w =
	List.fold_left (fun mp id -> MPdot(mp,Label.of_id id)) mp_mt idl
      in
      push_visible mp_mt [];
      let pp_w = str " with module " ++ pp_modname mp_w in
      pop_visible ();
      pp_module_type [] mt ++ pp_w ++ str " = " ++ pp_modname mp

let is_short = function MEident _ | MEapply _ -> true | _ -> false

let rec pp_structure_elem = function
  | (l,SEdecl d) ->
       (try
	 let ren = Common.check_duplicate (top_visible_mp ()) l in
	 hov 1 (str ("module "^ren^" = struct ") ++ fnl () ++ pp_decl d) ++
	 fnl () ++ str "end" ++ fnl () ++
	 pp_alias_decl ren d
	with Not_found -> pp_decl d)
  | (l,SEmodule m) ->
      let typ =
        (* virtual printing of the type, in order to have a correct mli later*)
	if Common.get_phase () == Pre then
	  str ": " ++ pp_module_type [] m.ml_mod_type
	else mt ()
      in
      let def = pp_module_expr [] m.ml_mod_expr in
      let name = pp_modname (MPdot (top_visible_mp (), l)) in
      hov 1
	(str "module " ++ name ++ typ ++ str " = " ++
	 (if (is_short m.ml_mod_expr) then mt () else fnl ()) ++ def) ++
      (try
	 let ren = Common.check_duplicate (top_visible_mp ()) l in
	 fnl () ++ str ("module "^ren^" = ") ++ name
       with Not_found -> mt ())
  | (l,SEmodtype m) ->
      let def = pp_module_type [] m in
      let name = pp_modname (MPdot (top_visible_mp (), l)) in
      hov 1 (str "module type " ++ name ++ str " = " ++ fnl () ++ def) ++
      (try
	 let ren = Common.check_duplicate (top_visible_mp ()) l in
         fnl () ++ str ("module type "^ren^" = ") ++ name
       with Not_found -> mt ())

and pp_module_expr params = function
  | MEident mp -> pp_modname mp
  | MEapply (me, me') ->
      pp_module_expr [] me ++ str "(" ++ pp_module_expr [] me' ++ str ")"
  | MEfunctor (mbid, mt, me) ->
      let name = pp_modname (MPbound mbid) in
      let typ = pp_module_type [] mt in
      let def = pp_module_expr (MPbound mbid :: params) me in
      str "functor (" ++ name ++ str ":" ++ typ ++ str ") ->" ++ fnl () ++ def
  | MEstruct (mp, sel) ->
      push_visible mp params;
      let l = List.map pp_structure_elem sel in
      pop_visible ();
      str "struct " ++ fnl () ++
      v 1 (str " " ++ prlist_with_sep fnl2 identity l) ++
      fnl () ++ str "end"

let do_struct f s =
  let pp s = try f s ++ fnl2 () with Failure "empty phrase" -> mt ()
  in
  let ppl (mp,sel) =
    push_visible mp [];
    let p = prlist_strict pp sel in
    (* for monolithic extraction, we try to simulate the unavailability
       of [MPfile] in names by artificially nesting these [MPfile] *)
    (if modular () then pop_visible ()); p
  in
  let p = prlist_strict ppl s in
  (if not (modular ()) then repeat (List.length s) pop_visible ());
  p

let pp_struct s = do_struct pp_structure_elem s

let pp_signature s = do_struct pp_specif s

let pp_decl d = try pp_decl d with Failure "empty phrase" -> mt ()

let ocaml_descr = {
  keywords = keywords;
  file_suffix = ".ml";
  preamble = preamble;
  pp_struct = pp_struct;
  sig_suffix = Some ".mli";
  sig_preamble = sig_preamble;
  pp_sig = pp_signature;
  pp_decl = pp_decl;
}


*)
