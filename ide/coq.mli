(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Coq : Interaction with the Coq toplevel *)

(** * Version and date *)

val short_version : unit -> string
val version : unit -> string

(** * Launch a test coqtop processes, ask for a correct coqtop if it fails.
    @return the list of arguments that coqtop did not understand
    (the files probably ..). This command may terminate coqide in
    case of trouble.  *)
val filter_coq_opts : string list -> string list

(** Launch a coqtop with the user args in order to be sure that it works,
    checking in particular that initial.coq is found. This command
    may terminate coqide in case of trouble *)
val check_connection : string list -> unit

(** * The structure describing a coqtop sub-process *)

type coqtop
type handle

(** * Coqtop tasks

  A task is a group of sequential calls to be perform on a coqtop.
  If a task is already sent to coqtop, it is considered busy
  ([is_computing] will answer [true]), and other task submission
  will be rejected.

  A task is represented as a continuation, with a coqtop [handle]
  as first argument, and a final inner continuation as 2nd argument.
  This inner continuation should be runned at the end of the task.
  Any exception occuring within the task will trigger a coqtop reset.
*)

type void
type task = handle -> (unit->void) -> void

(** * Starting / signaling / ending a real coqtop sub-process *)

val spawn_coqtop : string list -> coqtop
val close_coqtop : coqtop -> unit
val break_coqtop : coqtop -> unit
val reset_coqtop : coqtop -> unit
val is_computing : coqtop -> bool

type reset_kind = Planned | Unexpected
val set_reset_handler : coqtop -> (reset_kind -> task) -> unit

(** In win32, we'll use a different kill function than Unix.kill *)

val killer : (int -> unit) ref
val interrupter : (int -> unit) ref

(** [final_countdown] triggers an exit of coqide after
    some last cycles for closing remaining coqtop zombies *)

val final_countdown : unit -> unit

(** * Coqtop commmunication *)

(** Try to schedule a task on a coqtop. If coqtop is available, the task
    callback is run (asynchronously), otherwise the [(unit->unit)] callback
    is triggered.
    - If coqtop ever dies during the computation, this function restarts coqtop
      and calls the restart hook with the fresh coqtop.
    - If the argument function raises an exception, a coqtop reset occurs.
    - The task may be discarded if a [close_coqtop] or [reset_coqtop] occurs
      before its completion.
    - The task callback should run its inner continuation at the end. *)

val try_grab : coqtop -> task -> (unit -> unit) -> unit
val init_coqtop : coqtop -> task -> unit

(** * Atomic calls to coqtop

  These atomic calls can be combined to form arbitrary multi-call tasks.
  They correspond to the protocol calls (cf [Serialize] for more details).
  Note that each call is asynchronous: it will return immediately,
  but the inner callback will be executed later to handle the call answer
  when this answer is available. *)

type 'a atask = handle -> ('a Interface.value -> void) -> void

val interp :
  ?raw:bool -> ?verbose:bool -> string -> string atask
val rewind : int -> int atask
val status : Interface.status atask
val goals : Interface.goals option atask
val evars : Interface.evar list option atask
val hints : (Interface.hint list * Interface.hint) option atask
val inloadpath : string -> bool atask
val mkcases : string -> string list list atask

(** A specialized version of [raw_interp] dedicated to
    set/unset options. *)

module PrintOpt :
sig
  type t
  val implicit : t
  val coercions : t
  val raw_matching : t
  val notations : t
  val all_basic : t
  val existential : t
  val universes : t

  val set : (t * bool) list -> task
end
