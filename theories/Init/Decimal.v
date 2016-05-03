(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** * Decimal numbers *)

(** These numbers coded in base 10 will be used for parsing and printing
    other Coq numeral datatypes in an human-readable way.
    We represent numbers in base 10 as lists of decimal digits,
    in big-endian order (most significant digit comes first). *)

(** Unsigned integers are just lists of digits.
    For instance, ten is (D1 (D0 Nil)) *)

Inductive uint :=
 | Nil
 | D0 (_:uint)
 | D1 (_:uint)
 | D2 (_:uint)
 | D3 (_:uint)
 | D4 (_:uint)
 | D5 (_:uint)
 | D6 (_:uint)
 | D7 (_:uint)
 | D8 (_:uint)
 | D9 (_:uint).

(** For signed integers, we use two constructors [Pos] and [Neg]. *)

Inductive int := Pos (d:uint) | Neg (d:uint).

(** This representation favors simplicity over canonicity.
    For normalizing numbers, we need to remove the leading zero digits
    and fix the sign of zero *)

Fixpoint unorm d :=
  match d with
    | D0 d => unorm d
    | _ => d
  end.

Fixpoint norm d :=
  match d with
    | Pos d => Pos (unorm d)
    | Neg d =>
      match unorm d with
        | Nil => Pos Nil
        | d => Neg d
      end
  end.

(** A few easy operations. For more advanced computations, use the conversions
    with other Coq numeral datatypes (e.g. Z) and the operations on them. *)

Definition opp (d:int) :=
 match d with
   | Pos d => Neg d
   | Neg d => Pos d
 end.

(** For conversions with binary numbers, it is easier to operate
    on little-endian numbers. *)

Fixpoint rev (l l' : uint) :=
  match l with
  | Nil => l'
  | D0 l => rev l (D0 l')
  | D1 l => rev l (D1 l')
  | D2 l => rev l (D2 l')
  | D3 l => rev l (D3 l')
  | D4 l => rev l (D4 l')
  | D5 l => rev l (D5 l')
  | D6 l => rev l (D6 l')
  | D7 l => rev l (D7 l')
  | D8 l => rev l (D8 l')
  | D9 l => rev l (D9 l')
  end.

(** Doubling little-endian numbers *)

Fixpoint little_double d :=
  match d with
  | Nil => Nil
  | D0 l => D0 (little_double l)
  | D1 l => D2 (little_double l)
  | D2 l => D4 (little_double l)
  | D3 l => D6 (little_double l)
  | D4 l => D8 (little_double l)
  | D5 l => D0 (little_succ_double l)
  | D6 l => D2 (little_succ_double l)
  | D7 l => D4 (little_succ_double l)
  | D8 l => D6 (little_succ_double l)
  | D9 l => D8 (little_succ_double l)
  end

with little_succ_double d :=
  match d with
  | Nil => D1 Nil
  | D0 l => D1 (little_double l)
  | D1 l => D3 (little_double l)
  | D2 l => D5 (little_double l)
  | D3 l => D7 (little_double l)
  | D4 l => D9 (little_double l)
  | D5 l => D1 (little_succ_double l)
  | D6 l => D3 (little_succ_double l)
  | D7 l => D5 (little_succ_double l)
  | D8 l => D7 (little_succ_double l)
  | D9 l => D9 (little_succ_double l)
  end.
