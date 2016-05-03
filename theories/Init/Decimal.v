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

Fixpoint rev_app (l l' : uint) :=
  match l with
  | nil => l'
  | x :: l => rev_app l (x::l')
  end.

Fixpoint double_rev_carry (c:bool) l acc :=
  match l with
  | nil => if c then D1 :: acc else acc
  | D0 :: l => double_rev_carry false l ((if c then D1 else D0) :: acc)
  | D1 :: l => double_rev_carry false l ((if c then D3 else D2) :: acc)
  | D2 :: l => double_rev_carry false l ((if c then D5 else D4) :: acc)
  | D3 :: l => double_rev_carry false l ((if c then D7 else D6) :: acc)
  | D4 :: l => double_rev_carry false l ((if c then D9 else D8) :: acc)
  | D5 :: l => double_rev_carry true l ((if c then D1 else D0) :: acc)
  | D6 :: l => double_rev_carry true l ((if c then D3 else D2) :: acc)
  | D7 :: l => double_rev_carry true l ((if c then D5 else D4) :: acc)
  | D8 :: l => double_rev_carry true l ((if c then D7 else D6) :: acc)
  | D9 :: l => double_rev_carry true l ((if c then D9 else D8) :: acc)
  end.

Definition double d :=
  double_rev_carry false (rev_app d nil) nil.

Definition succ_double d :=
  double_rev_carry true (rev_app d nil) nil.

Fixpoint little_double d :=
  match d with
  | nil => nil
  | D0 :: l => D0 :: little_double l
  | D1 :: l => D2 :: little_double l
  | D2 :: l => D4 :: little_double l
  | D3 :: l => D6 :: little_double l
  | D4 :: l => D8 :: little_double l
  | D5 :: l => D0 :: little_succ_double l
  | D6 :: l => D2 :: little_succ_double l
  | D7 :: l => D4 :: little_succ_double l
  | D8 :: l => D6 :: little_succ_double l
  | D9 :: l => D8 :: little_succ_double l
  end

with little_succ_double d :=
  match d with
  | nil => D1 :: nil
  | D0 :: l => D1 :: little_double l
  | D1 :: l => D3 :: little_double l
  | D2 :: l => D5 :: little_double l
  | D3 :: l => D7 :: little_double l
  | D4 :: l => D9 :: little_double l
  | D5 :: l => D1 :: little_succ_double l
  | D6 :: l => D3 :: little_succ_double l
  | D7 :: l => D5 :: little_succ_double l
  | D8 :: l => D7 :: little_succ_double l
  | D9 :: l => D9 :: little_succ_double l
  end.
