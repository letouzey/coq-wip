(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i camlp4deps: "grammar/grammar.cma" i*)

open Pp
open Whelp

VERNAC ARGUMENT EXTEND whelp_constr_request
| [ "Match" ] -> [ "match" ]
| [ "Instance" ] -> [ "instance" ]
END

let mkind = Smartlocate.global_inductive_with_alias

let mkconstr c =
  let (sigma,env)= Lemmas.get_current_context () in
  snd (Constrintern.interp_open_constr sigma env c)

let getgoal () =
  let gls = Proof.V82.subgoals (Pfedit.get_pftreestate ()) in
  let gls = { gls with Evd.it = List.hd gls.Evd.it }  in
  Termops.it_mkNamedProd_or_LetIn (Tacmach.pf_concl gls) (Tacmach.pf_hyps gls)

VERNAC COMMAND EXTEND Whelp CLASSIFIED AS QUERY
| [ "Whelp" "Locate" string(s) ] -> [ whelp (Locate s) ]
| [ "Whelp" "Locate" preident(s) ] -> [ whelp (Locate s) ]
| [ "Whelp" "Elim" global(r) ] -> [ whelp (Elim (mkind r)) ]
| [ "Whelp" whelp_constr_request(req) constr(c) ] ->
  [ whelp (Constr (req, mkconstr c))]
END

VERNAC COMMAND EXTEND WhelpHint CLASSIFIED AS QUERY
| [ "Whelp" "Hint" constr(c) ] -> [ whelp (Constr ("hint", mkconstr c)) ]
| [ "Whelp" "Hint" ] => [ Vernacexpr.VtProofStep, Vernacexpr.VtLater ] ->
  [ whelp (Constr ("hint",getgoal ())) ]
END
