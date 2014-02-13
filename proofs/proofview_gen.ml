(* This file has been generated by extraction of bootstrap/Monads.v. It
   shouldn't be modified directly. To regenerate it run coqtop -batch
   -impredicative-set -l bootstrap/Monads.v in Coq's source directory. *)

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type ('a, 'b) list_view =
| Nil of exn
| Cons of 'a * 'b

module IOBase = 
 struct 
  type 'a coq_T = unit -> 'a
  
  type 'a coq_Ref = 'a Pervasives.ref
  
  (** val ret : 'a1 -> 'a1 coq_T **)
  
  let ret = fun a -> (); fun () -> a
  
  (** val bind : 'a1 coq_T -> ('a1 -> 'a2 coq_T) -> 'a2 coq_T **)
  
  let bind = fun a k -> (); fun () -> k (a ()) ()
  
  (** val ignore : 'a1 coq_T -> unit coq_T **)
  
  let ignore = fun a -> (); fun () -> ignore (a ())
  
  (** val seq : unit coq_T -> 'a1 coq_T -> 'a1 coq_T **)
  
  let seq = fun a k -> (); fun () -> a (); k ()
  
  (** val ref : 'a1 -> 'a1 coq_Ref coq_T **)
  
  let ref = fun a -> (); fun () -> Pervasives.ref a
  
  (** val set : 'a1 coq_Ref -> 'a1 -> unit coq_T **)
  
  let set = fun r a -> (); fun () -> Pervasives.(:=) r a
  
  (** val get : 'a1 coq_Ref -> 'a1 coq_T **)
  
  let get = fun r -> (); fun () -> Pervasives.(!) r
  
  (** val raise : exn -> 'a1 coq_T **)
  
  let raise = fun e -> (); fun () -> raise (Proof_errors.Exception e)
  
  (** val catch : 'a1 coq_T -> (exn -> 'a1 coq_T) -> 'a1 coq_T **)
  
  let catch = fun s h -> ();
  fun () -> try s ()
  with Proof_errors.Exception e as src ->
    let src = Err.push src in
    let e = Backtrace.app_backtrace ~src ~dst:e in
    h e ()
  
  (** val read_line : string coq_T **)
  
  let read_line = fun () -> try Pervasives.read_line () with e -> let e = Err.push e in raise e ()
  
  (** val print_char : char -> unit coq_T **)
  
  let print_char = fun c -> (); fun () -> print_char c
  
  (** val print : Pp.std_ppcmds -> unit coq_T **)
  
  let print = fun s -> (); fun () -> try Pp.pp s; Pp.pp_flush () with e -> let e = Err.push e in raise e ()
  
  (** val timeout : int -> 'a1 coq_T -> 'a1 coq_T **)
  
  let timeout = fun n t -> (); fun () ->
    let timeout_handler _ = Pervasives.raise (Proof_errors.Exception Proof_errors.Timeout) in
    let psh = Sys.signal Sys.sigalrm (Sys.Signal_handle timeout_handler) in
    Pervasives.ignore (Unix.alarm n);
    let restore_timeout () =
      Pervasives.ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm psh
    in
    try
      let res = t () in
      restore_timeout ();
      res
    with
    | e ->
      let e = Err.push e in
      restore_timeout ();
      Pervasives.raise e
 
 end

type proofview = { initial : (Term.constr*Term.types) list;
                   solution : Evd.evar_map; comb : Goal.goal list }

type logicalState = proofview

type logicalMessageType = bool*(Goal.goal list*Goal.goal list)

type logicalEnvironment = Environ.env

module NonLogical = 
 struct 
  type 'a t = 'a IOBase.coq_T
  
  type 'a ref = 'a IOBase.coq_Ref
  
  (** val ret : 'a1 -> 'a1 t **)
  
  let ret x =
    IOBase.ret x
  
  (** val bind : 'a1 t -> ('a1 -> 'a2 t) -> 'a2 t **)
  
  let bind x k =
    IOBase.bind x k
  
  (** val ignore : 'a1 t -> unit t **)
  
  let ignore x =
    IOBase.ignore x
  
  (** val seq : unit t -> 'a1 t -> 'a1 t **)
  
  let seq x k =
    IOBase.seq x k
  
  (** val new_ref : 'a1 -> 'a1 ref t **)
  
  let new_ref x =
    IOBase.ref x
  
  (** val set : 'a1 ref -> 'a1 -> unit t **)
  
  let set r x =
    IOBase.set r x
  
  (** val get : 'a1 ref -> 'a1 t **)
  
  let get r =
    IOBase.get r
  
  (** val raise : exn -> 'a1 t **)
  
  let raise e =
    IOBase.raise e
  
  (** val catch : 'a1 t -> (exn -> 'a1 t) -> 'a1 t **)
  
  let catch s h =
    IOBase.catch s h
  
  (** val timeout : int -> 'a1 t -> 'a1 t **)
  
  let timeout n x =
    IOBase.timeout n x
  
  (** val read_line : string t **)
  
  let read_line =
    IOBase.read_line
  
  (** val print_char : char -> unit t **)
  
  let print_char c =
    IOBase.print_char c
  
  (** val print : Pp.std_ppcmds -> unit t **)
  
  let print s =
    IOBase.print s
  
  (** val run : 'a1 t -> 'a1 **)
  
  let run = fun x ->
  try x () with Proof_errors.Exception e as src ->
    let src = Err.push src in
    let e = Backtrace.app_backtrace ~src ~dst:e in
    Pervasives.raise e
 end

module Logical = 
 struct 
  type 'a t =
    __ -> ('a -> proofview -> __ -> (__ -> __ -> (__ -> (bool*(Goal.goal
    list*Goal.goal list)) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
    IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ ->
    (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
    IOBase.coq_T) -> __ IOBase.coq_T) -> Environ.env -> __ -> (__ ->
    (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
    IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
    IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
    IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
    proofview -> __ -> (__ -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal
    list)) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
    (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
    IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
    IOBase.coq_T) -> Environ.env -> __ -> (__ -> (bool*(Goal.goal
    list*Goal.goal list)) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
    IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ ->
    (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
    IOBase.coq_T) -> __ IOBase.coq_T
  
  (** val ret :
      'a1 -> __ -> ('a1 -> proofview -> __ -> ('a2 -> __ -> (__ ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a2 -> __ -> ('a3 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a3 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a4 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a4 -> (exn -> 'a5
      IOBase.coq_T) -> 'a5 IOBase.coq_T) -> (exn -> 'a5 IOBase.coq_T) -> 'a5
      IOBase.coq_T **)
  
  let ret x =
    (); (fun _ k s -> Obj.magic k x s)
  
  (** val bind :
      'a1 t -> ('a1 -> 'a2 t) -> __ -> ('a2 -> proofview -> __ -> ('a3 -> __
      -> (__ -> (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn
      -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) ->
      __ IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a3 -> __ -> ('a4 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a4 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a5 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a5 -> (exn -> 'a6
      IOBase.coq_T) -> 'a6 IOBase.coq_T) -> (exn -> 'a6 IOBase.coq_T) -> 'a6
      IOBase.coq_T **)
  
  let bind x k =
    (); (fun _ k0 s -> Obj.magic x __ (fun a s' -> Obj.magic k a __ k0 s') s)
  
  (** val ignore :
      'a1 t -> __ -> (unit -> proofview -> __ -> ('a2 -> __ -> (__ ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a2 -> __ -> ('a3 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a3 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a4 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a4 -> (exn -> 'a5
      IOBase.coq_T) -> 'a5 IOBase.coq_T) -> (exn -> 'a5 IOBase.coq_T) -> 'a5
      IOBase.coq_T **)
  
  let ignore x =
    (); (fun _ k s -> Obj.magic x __ (fun x1 s' -> k () s') s)
  
  (** val seq :
      unit t -> 'a1 t -> __ -> ('a1 -> proofview -> __ -> ('a2 -> __ -> (__
      -> (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a2 -> __ -> ('a3 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a3 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a4 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a4 -> (exn -> 'a5
      IOBase.coq_T) -> 'a5 IOBase.coq_T) -> (exn -> 'a5 IOBase.coq_T) -> 'a5
      IOBase.coq_T **)
  
  let seq x k =
    (); (fun _ k0 s -> Obj.magic x __ (fun x1 s' -> Obj.magic k __ k0 s') s)
  
  (** val set :
      logicalState -> __ -> (unit -> proofview -> __ -> ('a1 -> __ -> (__ ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a1 -> __ -> ('a2 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a2 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a3 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a3 -> (exn -> 'a4
      IOBase.coq_T) -> 'a4 IOBase.coq_T) -> (exn -> 'a4 IOBase.coq_T) -> 'a4
      IOBase.coq_T **)
  
  let set s =
    (); (fun _ k x -> Obj.magic k () s)
  
  (** val get :
      __ -> (logicalState -> proofview -> __ -> ('a1 -> __ -> (__ ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a1 -> __ -> ('a2 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a2 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a3 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a3 -> (exn -> 'a4
      IOBase.coq_T) -> 'a4 IOBase.coq_T) -> (exn -> 'a4 IOBase.coq_T) -> 'a4
      IOBase.coq_T **)
  
  let get r k s =
    Obj.magic k s s
  
  (** val put :
      logicalMessageType -> __ -> (unit -> proofview -> __ -> ('a1 -> __ ->
      (__ -> (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a1 -> __ -> ('a2 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a2 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a3 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a3 -> (exn -> 'a4
      IOBase.coq_T) -> 'a4 IOBase.coq_T) -> (exn -> 'a4 IOBase.coq_T) -> 'a4
      IOBase.coq_T **)
  
  let put m =
    (); (fun _ k s _ k0 e _ k1 ->
      Obj.magic k () s __ k0 e __ (fun b c' ->
        k1 b
          ((if fst m then fst c' else false),((List.append (fst (snd m))
                                                (fst (snd c'))),(List.append
                                                                  (snd
                                                                    (snd m))
                                                                  (snd
                                                                    (snd c')))))))
  
  (** val current :
      __ -> (logicalEnvironment -> proofview -> __ -> ('a1 -> __ -> (__ ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a1 -> __ -> ('a2 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a2 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a3 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a3 -> (exn -> 'a4
      IOBase.coq_T) -> 'a4 IOBase.coq_T) -> (exn -> 'a4 IOBase.coq_T) -> 'a4
      IOBase.coq_T **)
  
  let current r k s r0 k0 e =
    Obj.magic k e s __ k0 e
  
  (** val zero :
      exn -> __ -> ('a1 -> proofview -> __ -> ('a2 -> __ -> (__ ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a2 -> __ -> ('a3 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a3 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a4 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a4 -> (exn -> 'a5
      IOBase.coq_T) -> 'a5 IOBase.coq_T) -> (exn -> 'a5 IOBase.coq_T) -> 'a5
      IOBase.coq_T **)
  
  let zero e =
    (); (fun _ k s _ k0 e0 _ k1 _ sk fk -> fk e)
  
  (** val plus :
      'a1 t -> (exn -> 'a1 t) -> __ -> ('a1 -> proofview -> __ -> ('a2 -> __
      -> (__ -> (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn
      -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) ->
      __ IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a2 -> __ -> ('a3 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a3 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a4 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a4 -> (exn -> 'a5
      IOBase.coq_T) -> 'a5 IOBase.coq_T) -> (exn -> 'a5 IOBase.coq_T) -> 'a5
      IOBase.coq_T **)
  
  let plus x y =
    (); (fun _ k s _ k0 e _ k1 _ sk fk ->
      Obj.magic x __ k s __ k0 e __ k1 __ sk (fun e0 ->
        Obj.magic y e0 __ k s __ k0 e __ k1 __ sk fk))
  
  (** val split :
      'a1 t -> __ -> (('a1, exn -> 'a1 t) list_view -> proofview -> __ ->
      ('a2 -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) -> __ ->
      (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> Environ.env -> __ -> (__ -> (bool*(Goal.goal
      list*Goal.goal list)) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ ->
      (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> proofview -> __ -> ('a2 -> __ ->
      ('a3 -> (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a3 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a4 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a4 -> (exn -> 'a5
      IOBase.coq_T) -> 'a5 IOBase.coq_T) -> (exn -> 'a5 IOBase.coq_T) -> 'a5
      IOBase.coq_T **)
  
  let split x =
    (); (fun _ k s _ k0 e _ k1 _ sk fk ->
      IOBase.bind
        (Obj.magic x __ (fun a s' _ k2 e0 -> k2 (a,s')) s __ (fun a _ k2 ->
          k2 a (true,([],[]))) e __ (fun a c _ sk0 fk0 -> sk0 (a,c) fk0) __
          (fun a fk0 ->
          IOBase.ret (Cons (a, (fun e0 _ sk0 fk1 ->
            IOBase.bind (fk0 e0) (fun x0 ->
              match x0 with
              | Nil e1 -> fk1 e1
              | Cons (a0, x1) -> sk0 a0 (fun e1 -> x1 e1 __ sk0 fk1))))))
          (fun e0 -> IOBase.ret (Nil e0))) (fun x0 ->
        match x0 with
        | Nil exc ->
          let c = true,([],[]) in
          Obj.magic k (Nil exc) s __ k0 e __ (fun b c' ->
            k1 b
              ((if fst c then fst c' else false),((List.append (fst (snd c))
                                                    (fst (snd c'))),(List.append
                                                                    (snd
                                                                    (snd c))
                                                                    (snd
                                                                    (snd c'))))))
            __ sk fk
        | Cons (p, y) ->
          let p0,m' = p in
          let a',s' = p0 in
          Obj.magic k (Cons (a', (fun exc _ k2 s0 _ k3 e0 _ k4 _ sk0 fk0 ->
            y exc __ (fun a fk1 ->
              let a0,c = a in
              let a1,s'0 = a0 in
              k2 a1 s'0 __ k3 e0 __ (fun b c' ->
                k4 b
                  ((if fst c then fst c' else false),((List.append
                                                        (fst (snd c))
                                                        (fst (snd c'))),
                  (List.append (snd (snd c)) (snd (snd c')))))) __ sk0 fk1)
              fk0))) s' __ k0 e __ (fun b c' ->
            k1 b
              ((if fst m' then fst c' else false),((List.append
                                                     (fst (snd m'))
                                                     (fst (snd c'))),
              (List.append (snd (snd m')) (snd (snd c')))))) __ sk fk))
  
  (** val lift :
      'a1 NonLogical.t -> __ -> ('a1 -> proofview -> __ -> ('a2 -> __ -> (__
      -> (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> (__ -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> (__ -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> proofview -> __ -> ('a2 -> __ -> ('a3 ->
      (bool*(Goal.goal list*Goal.goal list)) -> __ -> (__ -> (exn -> __
      IOBase.coq_T) -> __ IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> __ -> (__ -> (exn -> __ IOBase.coq_T) -> __
      IOBase.coq_T) -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) ->
      Environ.env -> __ -> ('a3 -> (bool*(Goal.goal list*Goal.goal list)) ->
      __ -> ('a4 -> (exn -> __ IOBase.coq_T) -> __ IOBase.coq_T) -> (exn ->
      __ IOBase.coq_T) -> __ IOBase.coq_T) -> __ -> ('a4 -> (exn -> 'a5
      IOBase.coq_T) -> 'a5 IOBase.coq_T) -> (exn -> 'a5 IOBase.coq_T) -> 'a5
      IOBase.coq_T **)
  
  let lift x =
    (); (fun _ k s _ k0 e _ k1 _ sk fk ->
      IOBase.bind x (fun x0 ->
        Obj.magic k x0 s __ k0 e __ (fun b c' ->
          k1 b
            ((if fst (true,([],[])) then fst c' else false),((List.append
                                                               (fst
                                                                 (snd
                                                                   (true,([],[]))))
                                                               (fst (snd c'))),
            (List.append (snd (snd (true,([],[])))) (snd (snd c')))))) __ sk
          fk))
  
  (** val run :
      'a1 t -> logicalEnvironment -> logicalState ->
      (('a1*logicalState)*logicalMessageType) NonLogical.t **)
  
  let run x e s =
    Obj.magic x __ (fun a s' _ k e0 -> k (a,s')) s __ (fun a _ k ->
      k a (true,([],[]))) e __ (fun a c _ sk fk -> sk (a,c) fk) __
      (fun a x0 -> IOBase.ret a) (fun e0 ->
      IOBase.raise ((fun e -> Proof_errors.TacticFailure e) e0))
 end

