(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** A pool structure : everything put in the pool stays there,
    and all objects put in the pool can be referred to by a index.
    This structure is meant to store things that cannot
    go in Summary (e.g. functions), while their indexes could be
    synchronized in Summary, since they are coded via integers).
    These pools will only grow during Coq lifetime. *)

module type TY = sig type t end

module type S = sig
  type t (** type of objects put in this pool *)
  type idx (** indexes used to retreive objects in the pool *)
  val put : t -> idx (** put an object somewhere in the pool *)
  val get : idx -> t (** read back the object at this index *)
end

module Make (T : TY) : S with type t = T.t
