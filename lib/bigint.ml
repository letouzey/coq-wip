(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Arbitrary large integer numbers. *)

(** NB : This is now a wrapper around OCaml's [Big_int] *)

open Big_int

type bigint = big_int

let of_string = big_int_of_string
let to_string = string_of_big_int

let of_int = big_int_of_int
let to_int = int_of_big_int

let zero = zero_big_int
let one = unit_big_int
let two = of_int 2

let add_1 = succ_big_int
let sub_1 = pred_big_int
let mult_2 = mult_int_big_int 2

let neg = minus_big_int

let add = add_big_int
let sub = sub_big_int
let mult = mult_big_int

let pow = power_big_int_positive_int

let less_than = lt_big_int
let equal = eq_big_int
let is_strictly_pos x = (sign_big_int x = 1)
let is_strictly_neg x = (sign_big_int x = -1)
let is_pos_or_zero x = (sign_big_int x >= 0)
let is_neg_or_zero x = (sign_big_int x <= 0)

let euclid n m =
  let n' = abs_big_int n and m' = abs_big_int m in
  let q',r' = quomod_big_int n' m' in
  (if sign_big_int n * sign_big_int m < 0 then neg q' else q'),
  (if sign_big_int n < 0 then neg r' else r')

(* Since OCaml 3.12.0, [Big_int] includes bitwise operartion such as
   [shift_right_towards_zero_big_int]. We could use then someday... *)

let div2_with_rest x =
   let q,r = euclid x two in
   q, (equal r one)

