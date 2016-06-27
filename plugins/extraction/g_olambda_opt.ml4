(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Extraction_plugin

(*i camlp4deps: "grammar/grammar.cma" i*)

let direct_eval ?(debug=false) (s:Miniml.ml_decl list) ot =
  Olambda_opt.eval_lambda ~debug (Olambda.lambda_for_eval s ot)

let compute_constr env c =
  try
    let ty = Retyping.get_type_of env Evd.empty c in
    let s,mlt,mlty = Extract_env.structure_for_compute c in
    let gterm = Olambda.reconstruct mlty (direct_eval s (Some mlt)) in
    Pretyping.understand
      ~flags:Pretyping.all_no_fail_flags
      ~expected_type:(Pretyping.OfType ty)
      env Evd.empty gterm
  with Olambda.CannotReconstruct r ->
    CErrors.error ("Cannot reconstruct a Coq value : " ^
                  Olambda.cannot_reconstruct_msg r)

let compute_constr_expr cexpr =
  let env = Global.env () in
  let c,_ = Constrintern.interp_constr env Evd.empty cexpr in
  let res,_ = compute_constr env c in
  Feedback.msg_notice (Printer.pr_lconstr res)

open Constrarg

VERNAC COMMAND EXTEND ExtractionCompute CLASSIFIED AS QUERY
| [ "Extraction" "Compute" constr(c) ]
  -> [ compute_constr_expr c ]
END

(* TO COMPILE:

make USERFLAGS="-I +compiler-libs" plugins/extraction/extrcompute_opt_plugin.cmx
ocamlopt -linkall -rectypes -shared -o test.cmxs -I +compiler-libs -I lib -I kernel -I library -I plugins/extraction ocamlcommon.cmxa ocamloptcomp.cmxa plugins/extraction/extrcompute_opt_plugin.cmx

*)
