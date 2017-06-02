(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** * DecimalNotations : the numeral notations for Decimal numbers *)
Numeral Notation Decimal.uint Decimal.uint_of_uint Decimal.uint_of_uint
  : uint_scope.
Numeral Notation Decimal.int Decimal.int_of_int Decimal.int_of_int
  : int_scope.
