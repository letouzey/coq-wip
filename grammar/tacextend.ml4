(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4deps: "tools/compat5b.cmo" i*)

open Util
open Pp
open Genarg
open Q_util
open Q_coqast
open Argextend
open Pcoq
open Egramml
open Compat

let rec make_patt = function
  | [] -> <:patt< [] >>
  | GramNonTerminal(loc',_,_,Some p)::l ->
      let p = Names.Id.to_string p in
      <:patt< [ $lid:p$ :: $make_patt l$ ] >>
  | _::l -> make_patt l

let rec make_when loc = function
  | [] -> <:expr< True >>
  | GramNonTerminal(loc',t,_,Some p)::l ->
      let loc' = of_coqloc loc' in
      let p = Names.Id.to_string p in
      let l = make_when loc l in
      let loc = CompatLoc.merge loc' loc in
      let t = mlexpr_of_argtype loc' t in
      <:expr< Genarg.argument_type_eq (Genarg.genarg_tag $lid:p$) $t$ && $l$ >>
  | _::l -> make_when loc l

let rec make_let raw e = function
  | [] -> <:expr< fun $lid:"ist"$ -> $e$ >>
  | GramNonTerminal(loc,t,_,Some p)::l ->
      let loc = of_coqloc loc in
      let p = Names.Id.to_string p in
      let loc = CompatLoc.merge loc (MLast.loc_of_expr e) in
      let e = make_let raw e l in
      let v =
        if raw then <:expr< Genarg.out_gen $make_rawwit loc t$ $lid:p$ >>
               else <:expr< Genarg.out_gen $make_topwit    loc t$ $lid:p$ >> in
      <:expr< let $lid:p$ = $v$ in $e$ >>
  | _::l -> make_let raw e l

let rec extract_signature = function
  | [] -> []
  | GramNonTerminal (_,t,_,_) :: l -> t :: extract_signature l
  | _::l -> extract_signature l

let check_unicity s l =
  let l' = List.map (fun (l,_,_) -> extract_signature l) l in
  if not (Util.List.distinct l') then
    Pp.msg_warning
      (strbrk ("Two distinct rules of tactic entry "^s^" have the same "^
      "non-terminals in the same order: put them in distinct tactic entries"))

let make_clause (pt,_,e) =
  (make_patt pt,
   vala (Some (make_when (MLast.loc_of_expr e) pt)),
   make_let false e pt)

let make_fun_clauses loc s l =
  check_unicity s l;
  Compat.make_fun loc (List.map make_clause l)

let rec make_args = function
  | [] -> <:expr< [] >>
  | GramNonTerminal(loc,t,_,Some p)::l ->
      let loc = of_coqloc loc in
      let p = Names.Id.to_string p in
      <:expr< [ Genarg.in_gen $make_topwit loc t$ $lid:p$ :: $make_args l$ ] >>
  | _::l -> make_args l

let mlexpr_terminals_of_grammar_tactic_prod_item_expr = function
  | GramTerminal s -> <:expr< Some $mlexpr_of_string s$ >>
  | GramNonTerminal (loc,nt,_,sopt) ->
    let loc = of_coqloc loc in <:expr< None >>

let make_prod_item = function
  | GramTerminal s -> <:expr< Egramml.GramTerminal $str:s$ >>
  | GramNonTerminal (loc,nt,g,sopt) ->
      let loc = of_coqloc loc in
      <:expr< Egramml.GramNonTerminal $default_loc$ $mlexpr_of_argtype loc nt$
      $mlexpr_of_prod_entry_key g$ $mlexpr_of_option mlexpr_of_ident sopt$ >>

let mlexpr_of_clause =
  mlexpr_of_list (fun (a,_,b) -> mlexpr_of_list make_prod_item a)

let rec make_tags loc = function
  | [] -> <:expr< [] >>
  | GramNonTerminal(loc',t,_,Some p)::l ->
      let loc' = of_coqloc loc' in
      let l = make_tags loc l in
      let loc = CompatLoc.merge loc' loc in
      let t = mlexpr_of_argtype loc' t in
      <:expr< [ $t$ :: $l$ ] >>
  | _::l -> make_tags loc l

let make_one_printing_rule se (pt,_,e) =
  let level = mlexpr_of_int 0 in (* only level 0 supported here *)
  let loc = MLast.loc_of_expr e in
  let prods = mlexpr_of_list mlexpr_terminals_of_grammar_tactic_prod_item_expr pt in
  <:expr< ($se$, { Pptactic.pptac_args = $make_tags loc pt$;
            pptac_prods = ($level$, $prods$) }) >>

let make_printing_rule se = mlexpr_of_list (make_one_printing_rule se)

let make_empty_check = function
| GramNonTerminal(_, t, e, _)->
  let is_extra = match t with ExtraArgType _ -> true | _ -> false in
  if is_possibly_empty e || is_extra then
    (* This possibly parses epsilon *)
    let wit = make_wit loc t in
    let rawwit = make_rawwit loc t in
    <:expr<
      match Genarg.default_empty_value $wit$ with
      [ None -> raise Exit
      | Some v ->
        Tacintern.intern_genarg Tacintern.fully_empty_glob_sign
          (Genarg.in_gen $rawwit$ v) ] >>
  else
  (* This does not parse epsilon (this Exit is static time) *)
    raise Exit
| GramTerminal _ ->
  (* Idem *)
  raise Exit

let rec possibly_empty_subentries loc = function
  | [] -> []
  | (s,prodsl) :: l ->
    let rec aux = function
    | [] -> (false,<:expr< None >>)
    | prods :: rest ->
      try
        let l = List.map make_empty_check prods in
        if has_extraarg prods then
          (true,<:expr< try Some $mlexpr_of_list (fun x -> x) l$
                        with [ Exit -> $snd (aux rest)$ ] >>)
        else
          (true, <:expr< Some $mlexpr_of_list (fun x -> x) l$ >>)
      with Exit -> aux rest in
    let (nonempty,v) = aux prodsl in
    if nonempty then (s,v) :: possibly_empty_subentries loc l
    else possibly_empty_subentries loc l

let possibly_atomic loc prods =
  let l = List.map_filter (function
    | GramTerminal s :: l, _, _ -> Some (s,l)
    | _ -> None) prods
  in
  possibly_empty_subentries loc (List.factorize_left String.equal l)

let declare_tactic loc s c cl =
  let se = mlexpr_of_string s in
  let pp = make_printing_rule se cl in
  let gl = mlexpr_of_clause cl in
  let atomic_tactics =
    mlexpr_of_list (mlexpr_of_pair mlexpr_of_string (fun x -> x))
      (possibly_atomic loc cl) in
  declare_str_items loc
    [ <:str_item< do {
      try do {
        Tacenv.register_ml_tactic $se$ $make_fun_clauses loc s cl$;
        List.iter
          (fun (s,l) -> match l with
           [ Some l ->
              Tacenv.register_atomic_ltac (Names.Id.of_string s)
              (Tacexpr.TacAtom($default_loc$,
                 Tacexpr.TacExtend($default_loc$,$se$,l)))
           | None -> () ])
          $atomic_tactics$ }
      with [ e when Err.noncritical e ->
	Pp.msg_warning
	  (Pp.app
	     (Pp.str ("Exception in tactic extend " ^ $se$ ^": "))
	     (Err.print e)) ];
      Egramml.extend_tactic_grammar $se$ $gl$;
      List.iter (fun (s, r) -> Pptactic.declare_ml_tactic_pprule s r) $pp$; } >>
    ]

open Pcaml
open PcamlSig

EXTEND
  GLOBAL: str_item;
  str_item:
    [ [ "TACTIC"; "EXTEND"; s = tac_name;
        c = OPT [ "CLASSIFIED"; "BY"; c = LIDENT -> <:expr< $lid:c$ >> ];
        OPT "|"; l = LIST1 tacrule SEP "|";
        "END" ->
         declare_tactic loc s c l ] ]
  ;
  tacrule:
    [ [ "["; l = LIST1 tacargs; "]";
        c = OPT [ "=>"; "["; c = Pcaml.expr; "]" -> c ];
        "->"; "["; e = Pcaml.expr; "]" ->
	(match l with
	  | GramNonTerminal _ :: _ ->
	    (* En attendant la syntaxe de tacticielles *)
	    failwith "Tactic syntax must start with an identifier"
	  | _ -> (l,c,e))
    ] ]
  ;
  tacargs:
    [ [ e = LIDENT; "("; s = LIDENT; ")" ->
        let t, g = interp_entry_name false None e "" in
        GramNonTerminal (!@loc, t, g, Some (Names.Id.of_string s))
      | e = LIDENT; "("; s = LIDENT; ","; sep = STRING; ")" ->
        let t, g = interp_entry_name false None e sep in
        GramNonTerminal (!@loc, t, g, Some (Names.Id.of_string s))
      | s = STRING ->
	if String.is_empty s then Err.user_err_loc (!@loc,"",Pp.str "Empty terminal.");
        GramTerminal s
    ] ]
  ;
  tac_name:
    [ [ s = LIDENT -> s
      | s = UIDENT -> s
    ] ]
  ;
  END
