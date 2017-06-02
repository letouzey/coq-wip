(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

Require Export Notations.
Require Export Logic.
Require Export Logic_Type.
Require Export Datatypes.
Require Export Specif.
Require Coq.Init.Decimal.
Require Coq.Init.Nat.
Require Export Peano.
Require Export Coq.Init.Wf.
Require Export Coq.Init.Tactics.
Require Export Coq.Init.Tauto.
(* Initially available plugins *)
Declare ML Module "extraction_plugin".
Declare ML Module "cc_plugin".
Declare ML Module "ground_plugin".
Declare ML Module "recdef_plugin".
Declare ML Module "numeral_notation_plugin".

(* Parsing / printing of [nat] numbers *)
Arguments Nat.of_uint d%uint_scope.
Arguments Nat.of_int d%int_scope.
Numeral Notation nat Nat.of_uint Nat.to_uint : nat_scope (abstract after 5000).

(* Default substrings not considered by queries like SearchAbout *)
Add Search Blacklist "_subproof" "_subterm" "Private_".
