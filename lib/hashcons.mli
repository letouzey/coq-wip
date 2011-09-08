(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2010     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Generic hash-consing. *)

module type Comp =
  sig
    type t
    type u
    val hash_sub :  u -> t -> t
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type S =
  sig
    type t
    type u
    val f : unit -> (u -> t -> t)
  end

module Make(X:Comp) : (S with type t = X.t and type u = X.u)

val simple_hcons : (unit -> 'u -> 't -> 't) -> ('u -> 't -> 't)

module Hstring : (S with type t = string and type u = unit)

