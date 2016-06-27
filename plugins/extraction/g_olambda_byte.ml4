(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4deps: "grammar/grammar.cma" i*)

open Extraction_plugin

let debug_compute = ref true (* TEMP! *)

let _ = Goptions.declare_bool_option
  {Goptions.optsync = true;
   Goptions.optdepr = false;
   Goptions.optname = "Verbose Extraction Compute";
   Goptions.optkey = ["Verbose";"Extraction";"Compute"];
   Goptions.optread = (fun () -> !debug_compute);
   Goptions.optwrite = ((:=) debug_compute) }

let make_cmo ?(debug=false) modulename (structure:Miniml.ml_decl list) =
  Olambda_byte.reset_compiler ();
  Olambda_byte.compile_lambda ~debug modulename
    (Olambda.lambda_for_compunit structure)

let direct_eval ?(debug=false) ?(opt=false) (s:Miniml.ml_decl list) ot =
  let lam = Olambda.lambda_for_eval s ot in
  if not opt then
    Olambda_byte.eval_lambda ~debug lam
  else
    let prg = "/home/letouzey/V8/plugins/extraction/ocamlopt_eval" in
    let prg = if debug then prg^" -debug" else prg in
    let i,o = Unix.open_process prg in
    let () = Marshal.to_channel o lam [] in
    let () = flush_all () in
    let res = Marshal.from_channel i in
    let _ = Unix.close_process (i,o) in
    res

let compute_constr ?(debug=false) ?(opt=false) env c =
  try
    let ty = Retyping.get_type_of env Evd.empty c in
    let s,mlt,mlty = Extract_env.structure_for_compute c in
    let gterm = Olambda.reconstruct mlty (direct_eval ~debug ~opt s (Some mlt))
    in
    Pretyping.understand
      ~flags:Pretyping.all_no_fail_flags
      ~expected_type:(Pretyping.OfType ty)
      env Evd.empty gterm
  with Olambda.CannotReconstruct r ->
    CErrors.error ("Cannot reconstruct a Coq value : " ^
                  Olambda.cannot_reconstruct_msg r)

let compute_constr_expr ?(debug=false) ?(opt=false) cexpr =
  let env = Global.env () in
  let c,_ = Constrintern.interp_constr env Evd.empty cexpr in
  let res,_ = compute_constr ~debug ~opt env c in
  Feedback.msg_notice (Printer.pr_lconstr res)

open Constrarg

VERNAC COMMAND EXTEND ExtractionCompute CLASSIFIED AS QUERY
| [ "Extraction" "Compute" constr(c) ]
  -> [ compute_constr_expr ~debug:(!debug_compute) c ]
END

VERNAC COMMAND EXTEND ExtractionNatCompute CLASSIFIED AS QUERY
| [ "Extraction" "NatCompute" constr(c) ]
  -> [ compute_constr_expr ~debug:(!debug_compute) ~opt:true c ]
END

(* To compile:

make USERFLAGS="-I +compiler-libs" plugins/extraction/extrcompute_byte_plugin.cmo

*)
