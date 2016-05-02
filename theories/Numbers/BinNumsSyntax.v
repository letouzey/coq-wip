(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** * Binary Numerical Datatypes *)

Require Import BinPosDef BinIntDef BinNatDef.
Require Decimal.

Local Open Scope N.

Definition digit2n d : N :=
  match d with
  | D0 => N0
  | D1 => Npos xH
  | D2 => Npos (xO xH)
  | D3 => Npos (xI xH)
  | D4 => Npos (xO (xO xH))
  | D5 => Npos (xI (xO xH))
  | D6 => Npos (xO (xI xH))
  | D7 => Npos (xI (xI xH))
  | D8 => Npos (xO (xO (xO xH)))
  | D9 => Npos (xI (xO (xO xH)))
  end.

Definition n2digit (n:N) :=
  match n with
  | 0 => D0
  | 1 => D1
  | 2 => D2
  | 3 => D3
  | 4 => D4
  | 5 => D5
  | 6 => D6
  | 7 => D7
  | 8 => D8
  | _ => D9 (* n>9 shouldn't happen *)
  end.

Fixpoint d2n (d:dec)(acc:N) :=
  match d with
  | nil => acc
  | d :: l => d2n l (N.add (digit2n d) (N.mul 10 acc))
  end.

Definition dec2n d := d2n d 0.

Fixpoint n2d (n:N)(acc:dec)(count:positive) :=
 match count, n with
 | xH, _ => acc
 | _, 0 => acc
 | xO count', _ | xI count', _ =>
     let (q,r) := N.div_eucl n 10 in
     n2d q (n2digit r :: acc) count'
 end.

Definition n2dec n :=
  n2d n nil (match n with 0 => xH | Npos p => xO p end).


Numeral Notation positive pos_of_Z' Z'_of_pos : positive_scope.
Numeral Notation N N_of_Z' Z'_of_N : N_scope.
Numeral Notation Z some_Z_of_Z' some_Z'_of_Z : Z_scope.
