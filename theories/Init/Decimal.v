(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

Require Import Datatypes.
Local Open Scope list_scope.

(** * Decimal numbers *)

(** These numbers coded in base 10 will be used for parsing and printing
    other Coq numeral datatypes in an human-readable way.
    We represent numbers in base 10 as lists of decimal digits,
    in big-endian order (most significant digit comes first). *)

Inductive digit := D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9.

(** Unsigned integers are just lists of digits.
    For instance, ten is D1 :: D0 :: nil *)

Definition uint := list digit.

(** For signed integers, we use two constructors [Pos] and [Neg]. *)

Inductive int := Pos (d:uint) | Neg (d:uint).

(** This representation favors simplicity over canonicity.
    For normalizing numbers, we need to remove the leading zero digits
    and fix the sign of zero *)

Fixpoint unorm d :=
  match d with
    | D0 :: d => unorm d
    | _ => d
  end.

Fixpoint norm d :=
  match d with
    | Pos d => Pos (unorm d)
    | Neg d =>
      match unorm d with
        | nil => Pos nil
        | d => Neg d
      end
  end.

(** A few easy operations. For more advanced computations, use the conversions
    with other Coq numeral datatypes (e.g. Z) and the operations on them. *)

Definition opp d :=
 match d with
   | Pos d => Neg d
   | Neg d => Pos d
 end.
