(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Abstract representations of values in a vo *)

(** NB: UPDATE THIS FILE EACH TIME cic.mli IS MODIFIED ! *)

(** We reify here the types of values present in a vo (see cic.mli),
    in order to validate its structure. Maybe this reification
    could become automatically generated someday ?

    - [Any] stands for a value that we won't check,
    - [Tuple] provides a name and sub-values in this block
    - [Sum] provides a name, a number of constant constructors,
      and sub-values at each position of each possible constructed
      variant
    - [List] and [Opt] could have been defined via [Sum], but
      having them here helps defining some recursive values below
    - [Annot] is a no-op, just there for improving debug messages *)

type value =
  | Any
  | Tuple of string * value array
  | Sum of string * int * value array array
  | Array of value
  | List of value
  | Opt of value
  | Int
  | String
  | Annot of string * value

(** Some pseudo-constructors *)

let v_tuple name v = Tuple(name,v)
let v_sum name cc vv = Sum(name,cc,vv)
let v_enum name n = Sum(name,n,[||])

(** Ocaml standard library *)

let v_bool = v_enum "bool" 2
let v_ref v = v_tuple "ref" [|v|]

let v_set v =
  let rec s = Sum ("Set.t",1,
    [|[|s; Annot("elem",v); s; Annot("bal",Int)|]|])
  in s

let v_map vk vd =
  let rec m = Sum ("Map.t",1,
    [|[|m; Annot("key",vk); Annot("data",vd); m; Annot("bal",Int)|]|])
  in m


(** kernel/names *)

let v_id = String
let v_dp = Annot ("dirpath", List v_id)
let v_name = v_sum "name" 1 [|[|v_id|]|]
let v_uid = v_tuple "uniq_ident" [|Int;String;v_dp|]
let rec v_mp = Sum("module_path",0,
  [|[|v_dp|];
    [|v_uid|];
    [|v_mp;v_id|]|])
let v_kn = v_tuple "kernel_name" [|v_mp;v_dp;v_id|]
let v_cst = v_sum "cst|mind" 0 [|[|v_kn|];[|v_kn;v_kn|]|]
let v_ind = v_tuple "inductive" [|v_cst;Int|]
let v_cons = v_tuple "constructor" [|v_ind;Int|]


(** kernel/univ *)

let v_level = v_sum "level" 1 [|[|Int;v_dp|]|]
let v_univ = v_sum "univ" 0
  [|[|v_level|];
    [|List v_level;List v_level|]|]

let v_cstrs =
  Annot
    ("Univ.constraints",
     v_set
       (v_tuple "univ_constraint"
          [|v_level;v_enum "order_request" 3;v_level|]))


(** kernel/term *)

let v_sort = v_sum "sort" 0 [|[|v_enum "cnt" 2|];[|v_univ|]|]
let v_sortfam = v_enum "sorts_family" 3

let v_caseinfo =
  let v_cstyle = v_enum "case_style" 5 in
  let v_cprint = v_tuple "case_printing" [|Int;v_cstyle|] in
  v_tuple "case_info" [|v_ind;Int;Array Int;v_cprint|]

let v_cast = v_enum "cast_kind" 3
(** NB : In fact there are 4 cast markers, but the last one
   (REVERTcast) isn't supposed to appear in a vo *)

let rec v_constr =
  Sum ("constr",0,[|
    [|Int|]; (* Rel *)
    [|v_id|]; (* Var *)
    [|Int|]; (* Meta *)
    [|v_evar|]; (* Evar *)
    [|v_sort|]; (* Sort *)
    [|v_constr;v_cast;v_constr|]; (* Cast *)
    [|v_name;v_constr;v_constr|]; (* Prod *)
    [|v_name;v_constr;v_constr|]; (* Lambda *)
    [|v_name;v_constr;v_constr;v_constr|]; (* LetIn *)
    [|v_constr;Array v_constr|]; (* App *)
    [|v_cst|]; (* Const *)
    [|v_ind|]; (* Ind *)
    [|v_cons|]; (* Construct *)
    [|v_caseinfo;v_constr;v_constr;Array v_constr|]; (* Case *)
    [|v_fix|]; (* Fix *)
    [|v_cofix|] (* CoFix *)
  |])

and v_evar = Tuple ("pexistential",[|Int;Array v_constr|])
and v_prec = Tuple ("prec_declaration",
                    [|Array v_name; Array v_constr; Array v_constr|])
and v_fix = Tuple ("pfixpoint", [|Tuple ("fix2",[|Array Int;Int|]);v_prec|])
and v_cofix = Tuple ("pcofixpoint",[|Int;v_prec|])


let v_rdecl = v_tuple "rel_declaration" [|v_name;Opt v_constr;v_constr|]
let v_rctxt = List v_rdecl

let v_section_ctxt = v_enum "emptylist" 1


(** kernel/mod_subst *)

let v_delta_hint =
  v_sum "delta_hint" 0 [|[|Int; Opt v_constr|];[|v_kn|]|]

let v_resolver =
  v_tuple "delta_resolver"
    [|v_map v_mp v_mp;
      v_map v_kn v_delta_hint|]

let v_mp_resolver = v_tuple "" [|v_mp;v_resolver|]

let v_subst =
  v_tuple "substitution"
    [|v_map v_mp v_mp_resolver;
      v_map v_uid v_mp_resolver|]


(** kernel/lazyconstr *)

let v_substituted v_a =
  v_ref
    (v_sum "constr_substituted" 0
      [|[|v_a|];[|List v_subst;v_a|]|])

let v_cstr_subst = v_substituted v_constr

(** NB: Second constructor [Direct] isn't supposed to appear in a .vo *)
let v_lazy_constr =
  v_sum "lazy_constr" 0 [|[|List v_subst;v_dp;Int|]|]


(** kernel/declarations *)

let v_engagement = v_enum "eng" 1

let v_pol_arity =
  v_tuple "polymorphic_arity" [|List(Opt v_univ);v_univ|]

let v_cst_type =
  v_sum "constant_type" 0 [|[|v_constr|];[|v_rctxt;v_pol_arity|]|]

let v_cst_def =
  v_sum "constant_def" 0
    [|[|Opt Int|]; [|v_cstr_subst|]; [|v_lazy_constr|]|]

let v_cb = v_tuple "constant_body"
  [|v_section_ctxt;
    v_cst_def;
    v_cst_type;
    Any;
    v_cstrs;
    Any;
    v_bool|]

let v_recarg = v_sum "recarg" 1 (* Norec *)
  [|[|v_ind|] (* Mrec *);[|v_ind|] (* Imbr *)|]

let rec v_wfp = Sum ("wf_paths",0,
    [|[|Int;Int|]; (* Rtree.Param *)
      [|v_recarg;Array v_wfp|]; (* Rtree.Node *)
      [|Int;Array v_wfp|] (* Rtree.Rec *)
    |])

let v_mono_ind_arity =
  v_tuple "monomorphic_inductive_arity" [|v_constr;v_sort|]

let v_ind_arity = v_sum "inductive_arity" 0
  [|[|v_mono_ind_arity|];[|v_pol_arity|]|]

let v_one_ind = v_tuple "one_inductive_body"
  [|v_id;
    v_rctxt;
    v_ind_arity;
    Array v_id;
    Array v_constr;
    Int;
    Int;
    List v_sortfam;
    Array v_constr;
    Array Int;
    v_wfp;
    Int;
    Int;
    Any|]

let v_ind_pack = v_tuple "mutual_inductive_body"
  [|Array v_one_ind;
    v_bool;
    v_bool;
    Int;
    v_section_ctxt;
    Int;
    Int;
    v_rctxt;
    v_cstrs;
    Any|]

let rec v_sfb =
  Sum ("struct_field_body",0,
  [|[|v_cb|];       (* SFBconst *)
    [|v_ind_pack|]; (* SFBmind *)
    [|v_module|];   (* SFBmodule *)
    [|v_modtype|]   (* SFBmodtype *)
  |])
and v_sb = List (Tuple ("label*sfb",[|v_id;v_sfb|]))
and v_seb =
  Sum ("struct_expr_body",0,
  [|[|v_mp|];                  (* SEBident *)
    [|v_uid;v_modtype;v_seb|]; (* SEBfunctor *)
    [|v_seb;v_seb;v_cstrs|];   (* SEBapply *)
    [|v_sb|];                  (* SEBstruct *)
    [|v_seb;v_with|]           (* SEBwith *)
  |])
and v_with =
  Sum ("with_declaration_body",0,
       [|[|List v_id;v_mp|];
         [|List v_id;v_cb|]|])
and v_module =
  Tuple ("module_body",
         [|v_mp;Opt v_seb;v_seb;
           Opt v_seb;v_cstrs;v_resolver;Any|])
and v_modtype =
  Tuple ("module_type_body",
         [|v_mp;v_seb;Opt v_seb;v_cstrs;v_resolver|])


(** kernel/safe_typing *)

let v_deps = Array (v_tuple "dep" [|v_dp;String|])
let v_compiled_lib =
  v_tuple "compiled" [|v_dp;v_module;v_deps;Opt v_engagement;Any|]

(** Library objects *)

let v_obj = Tuple ("Dyn.t",[|String;Any|])
let v_libobj = Tuple ("libobj", [|v_id;v_obj|])
let v_libobjs = List v_libobj
let v_libraryobjs = Tuple ("library_objects",[|v_mp;v_libobjs;v_libobjs|])

(** Main structures of a vo *)

let v_lib =
  Tuple ("library",[|v_dp;v_compiled_lib;v_libraryobjs;v_deps;Array v_dp|])

let v_opaques = Array v_constr
