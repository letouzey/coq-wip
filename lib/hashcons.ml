(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2010     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* Hash consing of datastructures *)

(* The generic hash-consing functions (does not use Obj) *)

(* [t] is the type of object to hash-cons
 * [u] is the type of hash-cons functions for the sub-structures
 *   of objects of type t (u usually has the form (t1->t1)*(t2->t2)*...).
 * [hash_sub u x] is a function that hash-cons the sub-structures of x using
 *   the hash-consing functions u provides.
 * [equal] is a comparison function. It is allowed to use physical equality
 *   on the sub-terms hash-consed by the hash_sub function.
 * [hash] is the hash function given to the Hashtbl.Make function
 *
 * Note that this module type coerces to the argument of Hashtbl.Make.
 *)

module type Comp =
  sig
    type t
    type u
    val hash_sub :  u -> t -> t
    val equal : t -> t -> bool
    val hash : t -> int
  end

(* The output is a function f such that
 * [f ()] has the side-effect of creating (internally) a hash-table of the
 *   hash-consed objects. The result is a function taking the sub-hashcons
 *   functions and an object, and hashcons it. It does not really make sense
 *   to call f() with different sub-hcons functions. That's why we use the
 *   wrappers simple_hcons, recursive_hcons, ... The latter just take as
 *   argument the sub-hcons functions (the tables are created at that moment),
 *   and returns the hcons function for t.
 *)

module type S =
  sig
    type t
    type u
    val f : unit -> (u -> t -> t)
  end

module Make(X:Comp) =
  struct
    type t = X.t
    type u = X.u

    (* We create the type of hashtables for t, with our comparison fun.
     * An invariant is that the table never contains two entries equals
     * w.r.t (=), although the equality on keys is X.equal. This is
     * granted since we hcons the subterms before looking up in the table.
     *)
    module Htbl = Hashtbl.Make(
      struct type t=X.t
             type u=X.u
             let hash=X.hash
             let equal x1 x2 = (*incr comparaison;*) X.equal x1 x2
      end)

    (* The table is created when () is applied.
     * Hashconsing is then very simple:
     *  1- hashcons the subterms using hash_sub and u
     *  2- look up in the table, if we do not get a hit, we add it
     *)
    let f () =
      let tab = Htbl.create 97 in
        (fun u x ->
           let y = X.hash_sub u x in
            (* incr acces;*)
             try let r = Htbl.find tab y in(* incr succes;*) r
             with Not_found -> Htbl.add tab y y; y)
  end

(* A few usefull wrappers:
 * takes as argument the function f above and build a function of type
 * u -> t -> t that creates a fresh table each time it is applied to the
 * sub-hcons functions. *)

(* For non-recursive types it is quite easy. *)
let simple_hcons h u = h () u

(* For a recursive type T, we write the module of sig Comp with u equals
 * to (T -> T) * u0
 * The first component will be used to hash-cons the recursive subterms
 * The second one to hashcons the other sub-structures.
 * We just have to take the fixpoint of h
 *)
let recursive_hcons h u =
  let hc = h () in
  let rec hrec x = hc (hrec,u) x in
  hrec

(* Basic hashcons modules for string and obj. Integers do not need be
   hashconsed.  *)

(* string *)
module Hstring = Make(
  struct
    type t = string
    type u = unit
    let hash_sub () s =(* incr accesstr;*) s
    let equal s1 s2 =(* incr comparaisonstr;
      if*) s1=s2(* then (incr successtr; true) else false*)
    let hash = Hashtbl.hash
  end)

let hcons_string = Hstring.f () ()


let no_scan_tag = 251
let tuple_p obj = Obj.is_block obj & (Obj.tag obj < no_scan_tag)

let hcons_inner_strings x =
  let rec do_iter o =
    if tuple_p o then
      let n = Obj.size o in
      for i = 0 to pred n do
	let o' = Obj.field o i in
	if Obj.is_block o' && Obj.tag o' = Obj.string_tag then
	  let s = (Obj.obj o' : string) in
	  let s' = hcons_string s in
	  if not (s == s') then begin
	    Printf.printf "Canonizing : %s\n%!" s;
	    Obj.set_field o i (Obj.repr s');
	  end
	else do_iter o'
      done
  in
  do_iter (Obj.repr x)
