(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4deps: "grammar/grammar.cma" i*)

let direct_eval ?(debug=false) (s:Miniml.ml_flat_structure) ot =
  Olambda_opt.eval_lambda ~debug (Olambda.lambda_for_eval s ot)

let compute_constr c =
  try
    let s,t,ty = Extract_env.structure_for_compute c in
    Olambda.reconstruct ty (direct_eval s (Some t))
  with Olambda.CannotReconstruct r ->
    Errors.error ("Cannot reconstruct a Coq value : " ^
                  Olambda.cannot_reconstruct_msg r)

let compute_constr_expr cexpr =
  let env = Global.env () in
  let c = Constrintern.interp_constr Evd.empty env cexpr in
  let res = compute_constr c in
  Pp.msg_notice (Printer.pr_lconstr res)

VERNAC COMMAND EXTEND ExtractionCompute CLASSIFIED AS QUERY
| [ "Extraction" "Compute" constr(c) ]
  -> [ compute_constr_expr c ]
END

(* TO COMPILE:

make USERFLAGS="-I +compiler-libs" plugins/extraction/extrcompute_opt_plugin.cmxa
ocamlopt -linkall -rectypes -shared -o test.cmxs -I +compiler-libs -I lib -I kernel -I library -I plugins/extraction ocamlcommon.cmxa ocamloptcomp.cmxa plugins/extraction/extrcompute_opt_plugin.cmxa

*)
