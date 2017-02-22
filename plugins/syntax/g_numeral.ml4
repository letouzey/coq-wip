(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

DECLARE PLUGIN "numeral_notation_plugin"

open Constrintern
open Pp
open CErrors
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

let rec pos_of_bigint posty n =
  match Bigint.div2_with_rest n with
  | (q, false) ->
      let c = mkConstruct (posty, 2) in (* xO *)
      mkApp (c, [| pos_of_bigint posty q |])
  | (q, true) when not (Bigint.equal q Bigint.zero) ->
      let c = mkConstruct (posty, 1) in (* xI *)
      mkApp (c, [| pos_of_bigint posty q |])
  | (q, true) ->
      mkConstruct (posty, 3) (* xH *)

let rec bigint_of_pos c = match Constr.kind c with
  | App (c, [| d |]) ->
      begin match Constr.kind c with
      | Construct ((_, n), _) ->
          begin match n with
          | 1 -> (* xI *) Bigint.add_1 (Bigint.mult_2 (bigint_of_pos d))
          | 2 -> (* xO *) Bigint.mult_2 (bigint_of_pos d)
          | n -> assert false
          end
      | x -> raise Not_found
      end
  | Construct ((_, 3), _) -> (* xH *) Bigint.one
  | x -> anomaly (str "bigint_of_pos" ++ str (obj_string x))

let z_of_bigint (zty, posty) ty thr n =
  if Bigint.is_pos_or_zero n && not (Bigint.equal thr Bigint.zero) &&
     Bigint.less_than thr n
  then
    Feedback.msg_warning
      (strbrk "Stack overflow or segmentation fault happens when " ++
       strbrk "working with large numbers in " ++
       str (string_of_reference ty) ++
       strbrk " (threshold may vary depending" ++
       strbrk " on your system limits and on the command executed).")
  else ();
  if not (Bigint.equal n Bigint.zero) then
    let (s, n) =
      if Bigint.is_pos_or_zero n then (2, n) (* Zpos *)
      else (3, Bigint.neg n) (* Zneg *)
    in
    let c = mkConstruct (zty, s) in
    mkApp (c, [| pos_of_bigint posty n |])
  else
    mkConstruct (zty, 1) (* Z0 *)

let bigint_of_z z = match Constr.kind z with
  | App (c, [| d |]) ->
      begin match Constr.kind c with
      | Construct ((_, n), _) ->
          begin match n with
          | 2 -> (* Zpos *) bigint_of_pos d
          | 3 -> (* Zneg *) Bigint.neg (bigint_of_pos d)
          | n -> assert false
          end
      | Const (c, _) -> anomaly (str "Const " ++ str (Constant.to_string c))
      | x -> anomaly (str "bigint_of_z App c " ++ str (obj_string x))
      end
  | Construct ((_, 1), _) -> (* Z0 *) Bigint.zero
  | _ -> raise Not_found

let constr_of_global_reference = function
  | VarRef v -> mkVar v
  | ConstRef cr -> mkConst cr
  | IndRef ind -> mkInd ind
  | ConstructRef c -> mkConstruct c

let rec constr_of_glob_constr vl = function
  | Glob_term.GRef (loc, gr, gllo) ->
      constr_of_global_reference gr
  | Glob_term.GVar (loc, id) ->
      constr_of_glob_constr vl (List.assoc id vl)
  | Glob_term.GApp (_, gc, gcl) ->
      let c = constr_of_glob_constr vl gc in
      let cl = List.map (constr_of_glob_constr vl) gcl in
      mkApp (c, Array.of_list cl)
  | _ ->
      raise Not_found

let rec glob_constr_of_constr loc c = match Constr.kind c with
  | Var id ->
      Glob_term.GRef (loc, VarRef id, None)
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

let interp_big_int zposty ty thr f loc bi =
  let t =
    let c = mkApp (mkConst f, [| z_of_bigint zposty ty thr bi |]) in
    eval_constr c
  in
  match Constr.kind t with
  | App (_, [| _; c |]) -> glob_constr_of_constr loc c
  | App (_, [| _ |]) ->
      CErrors.user_err ~loc
         (str "Cannot interpret this number as a value of type " ++
          str (string_of_reference ty))
  | x ->
      anomaly (str "interp_big_int " ++ str (obj_string x))

let uninterp_big_int loc g c =
  match try Some (constr_of_glob_constr [] c) with Not_found -> None with
  | Some c ->
      begin match
        try Some (eval_constr (mkApp (mkConst g, [| c |])))
        with Type_errors.TypeError _ -> None
      with
      | Some t ->
          begin match Constr.kind t with
          | App (c, [| _; x |]) -> Some (bigint_of_z x)
          | x -> None
          end
      | None ->
         None
      end
  | None ->
      None

let load_numeral_notation _ (_, (loc, zposty, ty, f, g, sc, patl, thr, path)) =
  Notation.declare_numeral_interpreter sc (path, [])
        (interp_big_int zposty ty thr f)
        (patl, uninterp_big_int loc g, true)

let cache_numeral_notation o = load_numeral_notation 1 o

type numeral_notation_obj =
  Loc.t * (Names.inductive * Names.inductive) *
  Libnames.reference * Names.constant *
  Names.constant *
  Notation_term.scope_name * Glob_term.glob_constr list *
  Bigint.bigint * Libnames.full_path

let inNumeralNotation : numeral_notation_obj -> Libobject.obj =
  Libobject.declare_object {
    (Libobject.default_object "NUMERAL NOTATION") with
    Libobject.cache_function = cache_numeral_notation;
    Libobject.load_function = load_numeral_notation }

let vernac_numeral_notation ty f g sc waft =
  let loc = Loc.ghost in
  let zposty =
    let zty =
      let c = qualid_of_ident (Id.of_string "Z") in
      try match Nametab.locate c with IndRef i -> i | _ -> raise Not_found
      with Not_found -> Nametab.error_global_not_found c
    in
    let positivety =
      let c = qualid_of_ident (Id.of_string "positive") in
      try match Nametab.locate c with IndRef i -> i | _ -> raise Not_found
      with Not_found -> Nametab.error_global_not_found c
    in
    (zty, positivety)
  in
  let tyc =
    let (loc, tyq) = qualid_of_reference ty in
    try Nametab.locate tyq with Not_found ->
      Nametab.error_global_not_found ~loc tyq
  in
  let fc =
    let (loc, fq) = qualid_of_reference f in
    try Nametab.locate_constant fq with Not_found ->
      Nametab.error_global_not_found ~loc fq
  in
  let lqid = qualid_of_reference ty in
  let crq = CRef (Qualid lqid, None) in
  let identref loc s = (loc, Names.Id.of_string s) in
  let app loc x y = CApp (loc, (None, x), [(y, None)]) in
  let cref loc s = CRef (Ident (identref loc s), None) in
  let arrow loc x y =
    CProdN (loc, [([(loc, Anonymous)], Default Decl_kinds.Explicit, x)], y)
  in
  let _ =
    (* checking "f" is of type "Z -> option ty" *)
    let c =
      CCast
        (loc, CRef (f, None),
         CastConv
           (arrow loc (cref loc "Z") (app loc (cref loc "option") crq)))
    in
    let (sigma, env) = Lemmas.get_current_context () in
    Constrintern.intern_constr env c
  in
  let thr = Bigint.of_int waft in
  let path = Nametab.path_of_global tyc in
  match tyc with
  | IndRef (sp, spi) ->
      let gc =
        let (loc, gq) = qualid_of_reference g in
        try Nametab.locate_constant gq with Not_found ->
          Nametab.error_global_not_found ~loc gq
      in
      let _ =
        (* checking "g" is of type "ty -> option Z" *)
        let c =
          CCast
            (loc, CRef (g, None),
             CastConv
               (arrow loc crq (app loc (cref loc "option") (cref loc "Z"))))
        in
        let (sigma, env) = Lemmas.get_current_context () in
        Constrintern.interp_open_constr env sigma c
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
           (loc, zposty, ty, fc, gc, sc, patl, thr, path))
  | ConstRef _ | ConstructRef _ | VarRef _ ->
      CErrors.user_err ~loc
        (str (string_of_reference ty) ++ str " is not an inductive type")

open Stdarg

VERNAC COMMAND EXTEND NumeralNotation CLASSIFIED AS SIDEFF
  | [ "Numeral" "Notation" reference(ty) reference(f) reference(g) ":"
      ident(sc) ] ->
    [ let waft = 0 in
      vernac_numeral_notation ty f g (Id.to_string sc) waft ]
  | [ "Numeral" "Notation" reference(ty) reference(f) reference(g) ":"
      ident(sc) "(" "warning" "after" int(waft) ")" ] ->
    [ vernac_numeral_notation ty f g (Id.to_string sc) waft ]
END
