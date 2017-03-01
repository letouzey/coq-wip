(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

DECLARE PLUGIN "numeral_notation_plugin"

open Pp
open Util
open Names
open Libnames
open Globnames
open Constrexpr
open Term
open Misctypes

(** * Numeral notation *)

let eval_constr (c : constr) =
  let env = Global.env () in
  let j = Arguments_renaming.rename_typing env c in
  Vnorm.cbv_vm env j.Environ.uj_val j.Environ.uj_type

exception NotANumber

let warning_big_num ty =
  strbrk "Stack overflow or segmentation fault happens when " ++
  strbrk "working with large numbers in " ++ pr_reference ty ++
  strbrk " (threshold may vary depending" ++
  strbrk " on your system limits and on the command executed)."

type conversion_function =
  | Direct of Names.constant
  | Option of Names.constant

(***********************************************************************)

(** ** Conversion between Coq [Decimal.int] and internal raw string *)

type int_ty =
  { uint : Names.inductive;
    int : Names.inductive }

(** Decimal.Nil has index 1, then Decimal.D0 has index 2 .. Decimal.D9 is 11 *)

let digit_of_char c =
  assert ('0' <= c && c <= '9');
  Char.code c - Char.code '0' + 2

let char_of_digit n =
  assert (2<=n && n<=11);
  Char.chr (n-2 + Char.code '0')

let coqint_of_rawnum inds (str,sign) =
  let nil = mkConstruct (inds.uint,1) in
  let rec do_chars s i acc =
    if i < 0 then acc
    else
      let dg = mkConstruct (inds.uint, digit_of_char s.[i]) in
      do_chars s (i-1) (mkApp(dg,[|acc|]))
  in
  let uint = do_chars str (String.length str - 1) nil in
  mkApp (mkConstruct (inds.int, if sign then 1 else 2), [|uint|])

let rawnum_of_coqint c =
  let rec of_uint_loop c buf =
    match Constr.kind c with
    | Construct ((_,1), _) (* Nil *) -> ()
    | App (c, [|a|]) ->
       (match Constr.kind c with
        | Construct ((_,n), _) (* D0 to D9 *) ->
           let () = Buffer.add_char buf (char_of_digit n) in
           of_uint_loop a buf
        | _ -> raise NotANumber)
    | _ -> raise NotANumber
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
      | _ -> raise NotANumber)
  | _ -> raise NotANumber


(***********************************************************************)

(** ** Conversion between Coq [Z] and internal bigint *)

type z_pos_ty =
  { z_ty : Names.inductive;
    pos_ty : Names.inductive }

(** First, [positive] from/to bigint *)

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
  | Construct ((_, 3), _) -> (* xH *) Bigint.one
  | App (c, [| d |]) ->
      begin match Constr.kind c with
      | Construct ((_, n), _) ->
          begin match n with
          | 1 -> (* xI *) Bigint.add_1 (Bigint.mult_2 (bigint_of_pos d))
          | 2 -> (* xO *) Bigint.mult_2 (bigint_of_pos d)
          | n -> assert false (* no other constructor of type positive *)
          end
      | x -> raise NotANumber
      end
  | x -> raise NotANumber

(** Now, [Z] from/to bigint *)

let maybe_warn (thr,msg) n =
  if Bigint.is_pos_or_zero n && not (Bigint.equal thr Bigint.zero) &&
     Bigint.less_than thr n
  then Feedback.msg_warning msg

let z_of_bigint { z_ty; pos_ty } warn n =
  maybe_warn warn n;
  if Bigint.equal n Bigint.zero then
    mkConstruct (z_ty, 1) (* Z0 *)
  else
    let (s, n) =
      if Bigint.is_pos_or_zero n then (2, n) (* Zpos *)
      else (3, Bigint.neg n) (* Zneg *)
    in
    let c = mkConstruct (z_ty, s) in
    mkApp (c, [| pos_of_bigint pos_ty n |])

let bigint_of_z z = match Constr.kind z with
  | Construct ((_, 1), _) -> (* Z0 *) Bigint.zero
  | App (c, [| d |]) ->
      begin match Constr.kind c with
      | Construct ((_, n), _) ->
          begin match n with
          | 2 -> (* Zpos *) bigint_of_pos d
          | 3 -> (* Zneg *) Bigint.neg (bigint_of_pos d)
          | n -> assert false (* no other constructor of type Z *)
          end
      | _ -> raise NotANumber
      end
  | _ -> raise NotANumber

(** The uinterp function below work at the level of [glob_constr]
   which is too low for us here. So here's a crude conversion back
   to [constr] for the subset that concerns us. *)

let rec constr_of_glob = function
  | Glob_term.GRef (_, ConstructRef c, _) -> mkConstruct c
  | Glob_term.GApp (_, gc, gcl) ->
      let c = constr_of_glob gc in
      let cl = List.map constr_of_glob gcl in
      mkApp (c, Array.of_list cl)
  | _ ->
      raise NotANumber

let rec glob_of_constr loc c = match Constr.kind c with
  | App (c, ca) ->
      let c = glob_of_constr loc c in
      let cel = List.map (glob_of_constr loc) (Array.to_list ca) in
      Glob_term.GApp (loc, c, cel)
  | Construct (c, _) -> Glob_term.GRef (loc, ConstructRef c, None)
  | Const (c, _) -> Glob_term.GRef (loc, ConstRef c, None)
  | Ind (ind, _) -> Glob_term.GRef (loc, IndRef ind, None)
  | Var id -> Glob_term.GRef (loc, VarRef id, None)
  | _ -> CErrors.anomaly (str "Numeral.interp_gen: unexpected constr")

let interp_gen ty coqify to_ty loc n =
  match to_ty with
  | Direct f ->
     let c = mkApp (mkConst f, [| coqify n |]) in
     glob_of_constr loc (eval_constr c)
  | Option f ->
     let c = mkApp (mkConst f, [| coqify n |]) in
     match Constr.kind (eval_constr c) with
     | App (_Some, [| _; c |]) -> glob_of_constr loc c
     | App (_None, [| _ |]) ->
        CErrors.user_err ~loc
         (str "Cannot interpret this number as a value of type " ++
          pr_reference ty)
     | x -> CErrors.anomaly (str "Numeral.interp_gen: option expected")

let interp_bigint zposty ty warn = interp_gen ty (z_of_bigint zposty warn)

(** TODO: restore the warning about large numbers in this version *)
let interp_rawnum inds ty = interp_gen ty (coqint_of_rawnum inds)

let uninterp_gen camlify of_ty n =
  try
    let t = constr_of_glob n in
    match of_ty with
    | Direct g ->
       let r = eval_constr (mkApp (mkConst g, [| t |])) in
       Some (camlify r)
    | Option g ->
       let r = eval_constr (mkApp (mkConst g, [| t |])) in
       match Constr.kind r with
       | App (_Some, [| _; x |]) -> Some (camlify x)
       | x -> None
  with
  | Type_errors.TypeError _ -> None (* cf. eval_constr *)
  | NotANumber -> None (* cf constr_of_glob or camlify *)

let uninterp_bigint = uninterp_gen bigint_of_z
let uninterp_rawnum = uninterp_gen rawnum_of_coqint

let big2raw n =
  if Bigint.is_pos_or_zero n then (Bigint.to_string n, true)
  else (Bigint.to_string (Bigint.neg n), false)

let raw2big (n,s) =
  if s then Bigint.of_string n else Bigint.neg (Bigint.of_string n)

type numeral_notation_obj =
  { num_ty : Libnames.reference;
    z_pos_ty : z_pos_ty option;
    int_ty : int_ty;
    to_ty : conversion_function;
    of_ty : conversion_function;
    raw_parse : bool; (* parsing from rawnum or from bigint ? *)
    raw_unparse : bool; (* unparsing from rawnum or from bigint ? *)
    scope : Notation_term.scope_name;
    constructors : Glob_term.glob_constr list;
    warning : Bigint.bigint * Pp.std_ppcmds;
    path : Libnames.full_path }

let load_numeral_notation _ (_, o) =
  Notation.declare_rawnumeral_interpreter o.scope (o.path, [])
  (if o.raw_parse then
     interp_rawnum o.int_ty o.num_ty o.to_ty
   else
     fun loc n ->
       interp_bigint (Option.get o.z_pos_ty) o.num_ty o.warning o.to_ty loc
         (raw2big n))
  (o.constructors,
   (if o.raw_unparse then
      uninterp_rawnum o.of_ty
    else
      fun n -> Option.map big2raw (uninterp_bigint o.of_ty n)),
   true)

let cache_numeral_notation o = load_numeral_notation 1 o

let inNumeralNotation : numeral_notation_obj -> Libobject.obj =
  Libobject.declare_object {
    (Libobject.default_object "NUMERAL NOTATION") with
    Libobject.cache_function = cache_numeral_notation;
    Libobject.load_function = load_numeral_notation }

let get_constructors ind =
  let open Declarations in
  let mib,oib = Global.lookup_inductive ind in
  let mc = oib.Declarations.mind_consnames in
  Array.to_list
    (Array.mapi
       (fun j c ->
         Glob_term.GRef
           (Loc.ghost, ConstructRef (ind, j + 1), None))
       mc)

let q_z = qualid_of_string "Coq.Numbers.BinNums.Z"
let q_positive = qualid_of_string "Coq.Numbers.BinNums.positive"
let q_int = qualid_of_string "Coq.Init.Decimal.int"
let q_uint = qualid_of_string "Coq.Init.Decimal.uint"
let q_option = qualid_of_string "Coq.Init.Datatypes.option"

let unsafe_locate_ind q =
  match Nametab.locate q with
  | IndRef i -> i
  | _ -> raise Not_found

let locate_ind q =
  try unsafe_locate_ind q
  with Not_found -> Nametab.error_global_not_found q

let locate_z () =
  try
    Some { z_ty = unsafe_locate_ind q_z;
           pos_ty = unsafe_locate_ind q_positive }
  with Not_found -> None

let locate_int () =
  { uint = locate_ind q_uint;
    int = locate_ind q_int }

let locate_globref r =
  let (loc, q) = qualid_of_reference r in
  try Nametab.locate q
  with Not_found -> Nametab.error_global_not_found ~loc q

let locate_constant r =
  let (loc, q) = qualid_of_reference r in
  try Nametab.locate_constant q
  with Not_found -> Nametab.error_global_not_found ~loc q

let has_type loc f ty =
  let c = CCast (loc, CRef (f, None), CastConv ty) in
  let (sigma, env) = Lemmas.get_current_context () in
  try
    ignore (Constrintern.interp_constr env sigma c); true
  with Pretype_errors.PretypeError _ -> false

let vernac_numeral_notation ty f g scope waft =
  let loc = Loc.ghost in
  let int_ty = locate_int () in
  let z_pos_ty = locate_z () in
  let tyc = locate_globref ty in
  let fc = locate_constant f in
  let gc = locate_constant g in
  let cty = CRef (ty, None) in
  let app x y = CApp (loc, (None, x), [(y, None)]) in
  let cref q = CRef (Qualid (loc,q), None) in
  let arrow x y =
    CProdN (loc, [([(loc, Anonymous)], Default Decl_kinds.Explicit, x)], y)
  in
  let cZ = cref q_z in
  let cint = cref q_int in
  let coption = cref q_option in
  let opt r = app coption r in
  (* Check that [ty] is an inductive type *)
  let constructors = match tyc with
    | IndRef ind -> get_constructors ind
    | ConstRef _ | ConstructRef _ | VarRef _ ->
       CErrors.user_err ~loc
        (pr_reference ty ++ str " is not an inductive type")
  in
  (* Check the type of f *)
  let raw_parse, to_ty =
    if has_type loc f (arrow cint cty) then
      true, Direct fc
    else if has_type loc f (arrow cint (opt cty)) then
      true, Option fc
    else if Option.is_empty z_pos_ty then
      CErrors.user_err ~loc
        (pr_reference f ++ str " should goes from Decimal.int to " ++
         pr_reference ty ++ str " or (option " ++ pr_reference ty ++
         str ")." ++ fnl () ++
         str "Instead of int, the type Z could also be used (load it first).")
    else if has_type loc f (arrow cZ cty) then
      false, Direct fc
    else if has_type loc f (arrow cZ (opt cty)) then
      false, Option fc
    else
      CErrors.user_err ~loc
        (pr_reference f ++ str " should goes from Decimal.int or Z to " ++
         pr_reference ty ++ str " or (option " ++ pr_reference ty ++ str ")")
  in
  (* Check the type of g *)
  let raw_unparse, of_ty =
    if has_type loc g (arrow cty cint) then
      true, Direct gc
    else if has_type loc g (arrow cty (opt cint)) then
      true, Option gc
    else if Option.is_empty z_pos_ty then
      CErrors.user_err ~loc
        (pr_reference g ++ str " should goes from " ++
         pr_reference ty ++
         str " to Decimal.int or (option int)." ++ fnl () ++
         str "Instead of int, the type Z could also be used (load it first).")
    else if has_type loc g (arrow cty cZ) then
      false, Direct gc
    else if has_type loc g (arrow cty (opt cZ)) then
      false, Option gc
    else
      CErrors.user_err ~loc
        (pr_reference g ++ str " should goes from " ++
         pr_reference ty ++
         str " to Decimal.int or (option int) or Z or (option Z)")
  in
  Lib.add_anonymous_leaf
    (inNumeralNotation
       { num_ty = ty;
         z_pos_ty;
         int_ty;
         to_ty;
         of_ty;
         raw_parse;
         raw_unparse;
         scope;
         constructors;
         warning = (Bigint.of_int waft, warning_big_num ty);
         path = Nametab.path_of_global tyc })

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
