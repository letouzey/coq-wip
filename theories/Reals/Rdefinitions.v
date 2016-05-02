(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*********************************************************)
(**          Definitions for the axiomatization          *)
(*********************************************************)

Require Export ZArith_base.

Parameter R : Set.

(* Declare Scope positive_scope with Key R *)
Delimit Scope R_scope with R.

(* Automatically open scope R_scope for arguments of type R *)
Bind Scope R_scope with R.

Local Open Scope R_scope.

Parameter R0 : R.
Parameter R1 : R.
Parameter Rplus : R -> R -> R.
Parameter Rmult : R -> R -> R.
Parameter Ropp : R -> R.
Parameter Rinv : R -> R.
Parameter Rlt : R -> R -> Prop.
Parameter up : R -> Z.

Infix "+" := Rplus : R_scope.
Infix "*" := Rmult : R_scope.
Notation "- x" := (Ropp x) : R_scope.
Notation "/ x" := (Rinv x) : R_scope.

Infix "<" := Rlt : R_scope.

(***********************************************************)

(**********)
Definition Rgt (r1 r2:R) : Prop := r2 < r1.

(**********)
Definition Rle (r1 r2:R) : Prop := r1 < r2 \/ r1 = r2.

(**********)
Definition Rge (r1 r2:R) : Prop := Rgt r1 r2 \/ r1 = r2.

(**********)
Definition Rminus (r1 r2:R) : R := r1 + - r2.

(**********)
Definition Rdiv (r1 r2:R) : R := r1 * / r2.

(**********)

Infix "-" := Rminus : R_scope.
Infix "/" := Rdiv   : R_scope.

Infix "<=" := Rle : R_scope.
Infix ">=" := Rge : R_scope.
Infix ">"  := Rgt : R_scope.

Notation "x <= y <= z" := (x <= y /\ y <= z) : R_scope.
Notation "x <= y < z"  := (x <= y /\ y <  z) : R_scope.
Notation "x < y < z"   := (x <  y /\ y <  z) : R_scope.
Notation "x < y <= z"  := (x <  y /\ y <= z) : R_scope.

(* Parsing and Printing digits strings as type R (when integer) *)

Fixpoint R_of_pos (p : positive) : R :=
  match p with
  | xH => R1
  | xO xH => Rplus R1 R1
  | xI xH => Rplus R1 (Rplus R1 R1)
  | xO q => Rmult (Rplus R1 R1) (R_of_pos q)
  | xI q => Rplus R1 (Rmult (Rplus R1 R1) (R_of_pos q))
  end.

Definition R_of_Z (z : Z) : option R :=
  match z with
  | Z0 => Some R0
  | Zpos p => Some (R_of_pos p)
  | Zneg p => Some (Ropp (R_of_pos p))
  end.

Definition R_of_dec (d : Decimal.int) : option R :=
 R_of_Z (Z.of_int d).

Ltac Z_of_posR2 r :=
  match r with
  | Rplus R1 R1 => constr: (2%Z)
  | Rplus R1 (Rplus R1 R1) => constr: (3%Z)
  | Rmult ?a ?b =>
      match Z_of_posR2 a with
      | 2%Z =>
          let b' := Z_of_posR2 b in
          eval compute in (Z.double b')
      end
  | Rplus R1 (Rmult ?a ?b) =>
      match Z_of_posR2 a with
      | 2%Z =>
           let b' := Z_of_posR2 b in
           eval compute in (Z.succ_double b')
      end
  end.

Ltac Z_of_posR r :=
  match r with
  | R0 => Z0
  | R1 => constr: (1%Z)
  | ?r => Z_of_posR2 r
  end.

Ltac Z_of_R r :=
  match r with
  | Ropp ?s =>
      match Z_of_posR s with
      | Z0 => fail
      | ?z => eval compute in (Z.opp z)
      end
  | _ =>
      Z_of_posR r
  end.

Ltac dec_of_R r :=
  let z := Z_of_R r in
  eval compute in (Z.to_int z).

Numeral Notation R R_of_dec dec_of_R : R_scope
  (printing Ropp R0 Rplus Rmult R1).
