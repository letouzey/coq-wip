(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4deps: "grammar/grammar.cma" i*)

open Constrintern
open Pp
open Errors
open Util
open Names
open Libnames
open Globnames
open Constrexpr
open Term
open Misctypes

(* Numeral notation *)

let obj_string x =
  if Obj.is_block (Obj.repr x) then
    "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
  else "int_val = " ^ string_of_int (Obj.magic x)

let eval_constr (c : constr) =
  let env = Global.env () in
  let j = Arguments_renaming.rename_typing env c in
  Vnorm.cbv_vm env j.Environ.uj_val j.Environ.uj_type

let eval_tacexpr ist env (te : Tacexpr.glob_tactic_expr) =
  let vft = Tacinterp.interp_ftactic ist te in
  let (_, pf) = Proofview.init Evd.empty [(env, mkProp)] in
  match
    try Some (Ftactic.apply env vft pf)
    with Logic_monad.TacticFailure _ -> None
  with
  | Some ([v], _, _, _) -> Some v
  | Some _ | None -> None

type coqinds =
    { uint : inductive;
      int : inductive }

let rawnum_to_coqint inds (str,sign) =
  let nil = mkConstruct (inds.uint,1) in
  let rec do_chars s i acc =
    if i < 0 then acc
    else
      let c = s.[i] in
      assert ('0' <= c && c <= '9');
      let dg = mkConstruct (inds.uint, Char.code c - Char.code '0' +2) in
      do_chars s (i-1) (mkApp(dg,[|acc|]))
  in
  let uint = do_chars str (String.length str - 1) nil in
  let int = mkApp (mkConstruct (inds.int, if sign then 1 else 2), [|uint|]) in
  int

let rawnum_of_coqint c =
  let rec of_uint_loop c buf =
    match Constr.kind c with
    | Construct ((_,1), _) (* Nil *) -> ()
    | App (c, [|a|]) ->
       (match Constr.kind c with
        | Construct ((_,n), _) (* D0 to D9 *) ->
           assert (0<=n-2 && n-2<=9);
           let () = Buffer.add_char buf (Char.chr (n-2 + Char.code '0')) in
           of_uint_loop a buf
        | _ -> raise Not_found)
    | _ -> raise Not_found
  in
  let of_uint c =
    let buf = Buffer.create 64 in
    let () = of_uint_loop c buf in
    if Int.equal (Buffer.length buf) 0 then "0" else Buffer.contents buf
  in
  match Constr.kind c with
  | App (c,[|c'|]) ->
     (match Constr.kind c with
      | Construct ((_,1), _) (* Pos *) -> (of_uint c', true)
      | Construct ((_,2), _) (* Neg *) -> (of_uint c', false)
      | _ -> raise Not_found)
  | _ -> raise Not_found

let constr_of_global_reference = function
  | VarRef v -> mkVar v
  | ConstRef cr -> mkConst cr
  | IndRef ind -> mkInd ind
  | ConstructRef c -> mkConstruct c

let rec constr_of_glob_constr = function
  | Glob_term.GRef (_, gr, gllo) -> constr_of_global_reference gr
  | Glob_term.GApp (_, gc, gcl) ->
      let c = constr_of_glob_constr gc in
      let cl = List.map constr_of_glob_constr gcl in
      mkApp (c, Array.of_list cl)
  | _ -> raise Not_found

let optconstr_of_glob_constr gc =
  try Some (constr_of_glob_constr gc) with Not_found -> None

let rec glob_constr_of_constr loc c = match Constr.kind c with
  | App (c, ca) ->
      let c = glob_constr_of_constr loc c in
      let cel = List.map (glob_constr_of_constr loc) (Array.to_list ca) in
      Glob_term.GApp (loc, c, cel)
  | Construct (c, _) ->
      Glob_term.GRef (loc, ConstructRef c, None)
  | Const (c, _) ->
      Glob_term.GRef (loc, ConstRef c, None)
  | Ind (ind, _) ->
      Glob_term.GRef (loc, IndRef ind, None)
  | x ->
      anomaly (str "1 constr " ++ str (obj_string x))

(** TODO: restore this warning above threshold *)

let interp_coqint inds ty thr (f,total) loc rawnum =
  let t =
    let c = mkApp (mkConst f, [| rawnum_to_coqint inds rawnum |]) in
    eval_constr c
  in
  if total then glob_constr_of_constr loc t
  else
    match Constr.kind t with
    | App (_, [| _; c |]) (* Some *) -> glob_constr_of_constr loc c
    | App (_, [| _ |]) (* None *) ->
       user_err_loc
         (loc, "_",
          str "Cannot interpret this number as a value of type " ++
            str (string_of_reference ty))
    | x ->
       anomaly (str "interp_coqint " ++ str (obj_string x))

let uninterp_coqint loc (g,total) c =
  match optconstr_of_glob_constr c with
  | None -> None
  | Some c ->
     try
       let c = eval_constr (mkApp (mkConst g, [| c |])) in
       if total then Some (rawnum_of_coqint c)
       else
         match Constr.kind c with
         | App (_, [| _; c |]) (* Some *) -> Some (rawnum_of_coqint c)
         | _ -> None
     with Type_errors.TypeError _ -> None

let uninterp_coqint_ltac tac c =
  let c = (c, None) in
  let loc = Loc.ghost in
  let c = Tacexpr.ConstrMayEval (Genredexpr.ConstrTerm c) in
  let ta = Tacexpr.TacCall (loc, ArgArg (loc, tac), [c]) in
  let te = Tacexpr.TacArg (loc, ta) in
  match eval_tacexpr (Tacinterp.default_ist ()) (Global.env ()) te with
  | Some v ->
      begin match Tacinterp.Value.to_constr v with
      | Some c -> Some (rawnum_of_coqint c)
      | None -> None
      end
  | None ->
      None

let load_numeral_notation _ (_, (loc, inds, ty, f, g, sc, patl, thr, path)) =
  match g with
  | Inl g ->
      Notation.declare_rawnumeral_interpreter sc (path, [])
        (interp_coqint inds ty thr f)
	(patl, uninterp_coqint loc g, true)
  | Inr ltac ->
      Notation.declare_rawnumeral_interpreter sc (path, [])
        (interp_coqint inds ty thr f)
	(patl, uninterp_coqint_ltac ltac, false)

let cache_numeral_notation o = load_numeral_notation 1 o

type numeral_notation_obj =
  Loc.t * coqinds *
  Libnames.reference * (Names.constant * bool) *
  (Names.constant * bool, Nametab.ltac_constant) union *
  Notation_term.scope_name * Glob_term.glob_constr list *
  Bigint.bigint * Libnames.full_path

let inNumeralNotation : numeral_notation_obj -> Libobject.obj =
  Libobject.declare_object {
    (Libobject.default_object "NUMERAL NOTATION") with
    Libobject.cache_function = cache_numeral_notation;
    Libobject.load_function = load_numeral_notation }

let locate (loc,q) =
  try Nametab.locate q
  with Not_found -> Nametab.error_global_not_found_loc loc q

let locate_ind (loc,q) =
  try match Nametab.locate q with IndRef i -> i | _ -> raise Not_found
  with Not_found -> Nametab.error_global_not_found_loc loc q

let locate_cst (loc,q) =
  try Nametab.locate_constant q
  with Not_found -> Nametab.error_global_not_found_loc loc q

let vernac_numeral_notation ty f g sc patl waft =
  let loc = Loc.ghost in
  let uint = loc,qualid_of_string "Coq.Init.Decimal.uint" in
  let int = loc,qualid_of_string "Coq.Init.Decimal.int" in
  let inds = { uint = locate_ind uint; int = locate_ind int } in
  let tyq = qualid_of_reference ty in
  let tyc = locate tyq in
  let fc = locate_cst (qualid_of_reference f) in
  let crq q = CRef (Qualid q, None) in
  let identref loc s = (loc, Names.Id.of_string s) in
  let app loc x y = CApp (loc, (None, x), [(y, None)]) in
  let cref loc s = CRef (Ident (identref loc s), None) in
  let arrow loc x y =
    CProdN (loc, [([(loc, Anonymous)], Default Decl_kinds.Explicit, x)], y)
  in
  let totalf =
    (* checking whether "f" is  of type "Decimal.int -> ty" *)
    (* TODO : pourquoi via des constrexpr ? *)
    let env = snd (Lemmas.get_current_context ()) in
    try
      let c =
        CCast
          (loc, CRef (f, None),
           CastConv (arrow loc (crq int) (crq tyq)))
      in
      let _ = Constrintern.intern_constr env c in
      true
    with e when Errors.noncritical e ->
      (* checking whether "f" is  of type "Decimal.int -> option ty" *)
      let c =
        CCast
          (loc, CRef (f, None),
           CastConv
             (arrow loc (crq int) (app loc (cref loc "option") (crq tyq))))
      in
      let _ = Constrintern.intern_constr env c in
      false
  in
  let thr = Bigint.of_int waft in
  let path = Nametab.path_of_global tyc in
  match (tyc, patl) with
  | (IndRef (sp, spi), []) ->
      let gc =
        let (loc, gq) = qualid_of_reference g in
        try Nametab.locate_constant gq with Not_found ->
          Nametab.error_global_not_found_loc loc gq
      in
      let totalg =
        let (sigma, env) = Lemmas.get_current_context () in
        try
          (* checking "g" is of type "ty -> Decimal.int" *)
          let c =
            CCast
              (loc, CRef (g, None),
               CastConv (arrow loc (crq tyq) (crq int)))
          in
          let _ = Constrintern.interp_open_constr env sigma c in
          true
        with e when Errors.noncritical e ->
          (* checking "g" is of type "ty -> option Decimal.int" *)
          let c =
            CCast
              (loc, CRef (g, None),
               CastConv
                 (arrow loc (crq tyq) (app loc (cref loc "option") (crq int))))
          in
          let _ = Constrintern.interp_open_constr env sigma c in
          false
      in
      let env = Global.env () in
      let patl =
        let mc =
          let mib = Environ.lookup_mind sp env in
          let inds =
            List.init (Array.length mib.Declarations.mind_packets)
                      (fun x -> (sp, x))
          in
          let ind = List.hd inds in
          let mip = mib.Declarations.mind_packets.(snd ind) in
          mip.Declarations.mind_consnames
        in
        Array.to_list
          (Array.mapi
             (fun i c ->
              Glob_term.GRef
                (loc, ConstructRef ((sp, spi), i + 1), None))
             mc)
      in
      Lib.add_anonymous_leaf
        (inNumeralNotation
	   (loc, inds, ty, (fc,totalf), Inl (gc,totalg), sc, patl, thr, path))
  | ((IndRef _ | ConstRef _), _) ->
      let gc =
        let (loc, gq) = qualid_of_reference g in
        try Nametab.locate_tactic gq with Not_found ->
          user_err_loc
            (loc, "_",
             str "tactic " ++ str (string_of_qualid gq) ++ str " not found")
      in
      let patl =
        match patl with
        | _ :: _ ->
            List.map (fun r -> Glob_term.GRef (loc, intern_reference r, None))
              patl
        | [] -> []
      in
      Lib.add_anonymous_leaf
        (inNumeralNotation
	   (loc, inds, ty, (fc,totalf), Inr gc, sc, patl, thr, path))
  | (VarRef _, _) | (ConstructRef _, _) ->
      user_err_loc
        (loc, "_", str (string_of_reference ty) ++ str " is not a type")

open Constrarg
open Stdarg

VERNAC COMMAND EXTEND NumeralNotation CLASSIFIED AS QUERY
  | [ "Numeral" "Notation" reference(ty) reference(f) reference(g) ":"
      ident(sc) ] ->
    [ let (patl, waft) = ([], 0) in
      vernac_numeral_notation ty f g (Id.to_string sc) patl waft ]
  | [ "Numeral" "Notation" reference(ty) reference(f) reference(g) ":"
      ident(sc) "(" "printing" reference_list(patl) ")" ] ->
    [ let waft = 0 in
      vernac_numeral_notation ty f g (Id.to_string sc) patl waft ]
  | [ "Numeral" "Notation" reference(ty) reference(f) reference(g) ":"
      ident(sc) "(" "warning" "after" int(waft) ")" ] ->
    [ let patl = [] in
      vernac_numeral_notation ty f g (Id.to_string sc) patl waft ]
  | [ "Numeral" "Notation" reference(ty) reference(f) reference(g) ":"
      ident(sc) "(" "printing" reference_list(patl) ")"
      "(" "warning" "after" int(waft) ")" ] ->
    [ vernac_numeral_notation ty f g (Id.to_string sc) patl waft ]
END
