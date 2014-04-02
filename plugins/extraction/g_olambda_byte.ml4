(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4deps: "grammar/grammar.cma" i*)

let make_cmo ?(debug=false) modulename (structure:Miniml.ml_decl list) =
  Olambda_byte.reset_compiler ();
  Olambda_byte.compile_lambda ~debug modulename
    (Olambda.lambda_for_compunit structure)

let direct_eval ?(debug=false) (s:Miniml.ml_decl list) ot =
  Olambda_byte.eval_lambda ~debug (Olambda.lambda_for_eval s ot)

let compute_constr env c =
  try
    let ty = Retyping.get_type_of env Evd.empty c in
    let s,mlt,mlty = Extract_env.structure_for_compute c in
    let gterm = Olambda.reconstruct mlty (direct_eval s (Some mlt)) in
    Pretyping.understand
      ~flags:Pretyping.all_no_fail_flags
      ~expected_type:(Pretyping.OfType ty)
      Evd.empty env gterm
  with Olambda.CannotReconstruct r ->
    Errors.error ("Cannot reconstruct a Coq value : " ^
                  Olambda.cannot_reconstruct_msg r)

let compute_constr_expr cexpr =
  let env = Global.env () in
  let c = Constrintern.interp_constr Evd.empty env cexpr in
  let res = compute_constr env c in
  Pp.msg_notice (Printer.pr_lconstr res)

VERNAC COMMAND EXTEND ExtractionCompute CLASSIFIED AS QUERY
| [ "Extraction" "Compute" constr(c) ]
  -> [ compute_constr_expr c ]
END
