(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Ideutils
open Preferences

(** * Version and date *)

let get_version_date () =
  let date =
    if Glib.Utf8.validate Coq_config.date
    then Coq_config.date
    else "<date not printable>" in
  try
    (* the following makes sense only when running with local layout *)
    let coqroot = Filename.concat
      (Filename.dirname Sys.executable_name)
      Filename.parent_dir_name
    in
    let ch = open_in (Filename.concat coqroot "revision") in
    let ver = input_line ch in
    let rev = input_line ch in
    (ver,rev)
  with _ -> (Coq_config.version,date)

let short_version () =
  let (ver,date) = get_version_date () in
  Printf.sprintf "The Coq Proof Assistant, version %s (%s)\n" ver date

let version () =
  let (ver,date) = get_version_date () in
    Printf.sprintf
      "The Coq Proof Assistant, version %s (%s)\
       \nArchitecture %s running %s operating system\
       \nGtk version is %s\
       \nThis is %s (%s is the best one for this architecture and OS)\
       \n"
      ver date
      Coq_config.arch Sys.os_type
      (let x,y,z = GMain.Main.version in Printf.sprintf "%d.%d.%d" x y z)
      (Filename.basename Sys.executable_name)
      Coq_config.best


(** * Initial checks by launching test coqtop processes *)

let rec read_all_lines in_chan =
  try
    let arg = input_line in_chan in
    arg::(read_all_lines in_chan)
  with End_of_file -> []

let fatal_error_popup msg =
  let popup = GWindow.message_dialog ~buttons:GWindow.Buttons.ok
    ~message_type:`ERROR ~message:msg ()
  in ignore (popup#run ()); exit 1

let final_info_popup small msg =
  if small then
    let popup = GWindow.message_dialog ~buttons:GWindow.Buttons.ok
      ~message_type:`INFO ~message:msg ()
    in
    let _ = popup#run () in
    exit 0
  else
    let popup = GWindow.dialog () in
    let button = GButton.button ~label:"ok" ~packing:popup#action_area#add ()
    in
    let scroll = GBin.scrolled_window ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
      ~packing:popup#vbox#add ~height:500 ()
    in
    let _ = GMisc.label ~text:msg ~packing:scroll#add_with_viewport () in
    let _ = popup#connect#destroy ~callback:(fun _ -> exit 0) in
    let _ = button#connect#clicked ~callback:(fun _ -> exit 0) in
    let _ = popup#run () in
    exit 0

let connection_error cmd lines exn =
  fatal_error_popup
    ("Connection with coqtop failed!\n"^
     "Command was: "^cmd^"\n"^
     "Answer was: "^(String.concat "\n  " lines)^"\n"^
     "Exception was: "^Printexc.to_string exn)

let display_coqtop_answer cmd lines =
  final_info_popup (List.length lines < 30)
    ("Coqtop exited\n"^
     "Command was: "^cmd^"\n"^
     "Answer was: "^(String.concat "\n  " lines))

let check_remaining_opt arg =
  if arg <> "" && arg.[0] = '-' then fatal_error_popup ("Illegal option: "^arg)

let rec filter_coq_opts args =
  let argstr = String.concat " " (List.map Filename.quote args) in
  let cmd = Filename.quote (coqtop_path ()) ^" -nois -filteropts " ^ argstr in
  let cmd = requote cmd in
  let filtered_args = ref [] in
  let errlines = ref [] in
  try
    let oc,ic,ec = Unix.open_process_full cmd (Unix.environment ()) in
    filtered_args := read_all_lines oc;
    errlines := read_all_lines ec;
    match Unix.close_process_full (oc,ic,ec) with
      | Unix.WEXITED 0 ->
	List.iter check_remaining_opt !filtered_args; !filtered_args
      | Unix.WEXITED 127 -> asks_for_coqtop args
      | _ -> display_coqtop_answer cmd (!filtered_args @ !errlines)
  with Sys_error _ -> asks_for_coqtop args
    | e -> connection_error cmd (!filtered_args @ !errlines) e

and asks_for_coqtop args =
  let pb_mes = GWindow.message_dialog
    ~message:"Failed to load coqtop. Reset the preference to default ?"
    ~message_type:`QUESTION ~buttons:GWindow.Buttons.yes_no () in
  match pb_mes#run () with
    | `YES ->
      let () = current.cmd_coqtop  <- None in
      let () = custom_coqtop := None in
      let () = pb_mes#destroy () in
      filter_coq_opts args
    | `DELETE_EVENT | `NO ->
      let () = pb_mes#destroy () in
      let cmd_sel = GWindow.file_selection
	~title:"Coqtop to execute (edit your preference then)"
	~filename:(coqtop_path ()) ~urgency_hint:true () in
      match cmd_sel#run () with
	| `OK ->
	  let () = custom_coqtop := (Some cmd_sel#filename) in
	  let () = cmd_sel#destroy () in
	  filter_coq_opts args
	| `CANCEL | `DELETE_EVENT | `HELP -> exit 0

exception WrongExitStatus of string

let print_status = function
  | Unix.WEXITED n -> "WEXITED "^string_of_int n
  | Unix.WSIGNALED n -> "WSIGNALED "^string_of_int n
  | Unix.WSTOPPED n -> "WSTOPPED "^string_of_int n

let check_connection args =
  let lines = ref [] in
  let argstr = String.concat " " (List.map Filename.quote args) in
  let cmd = Filename.quote (coqtop_path ()) ^ " -batch " ^ argstr in
  let cmd = requote cmd in
  try
    let ic = Unix.open_process_in cmd in
    lines := read_all_lines ic;
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> () (* coqtop seems ok *)
    | st -> raise (WrongExitStatus (print_status st))
  with e -> connection_error cmd !lines e

(** Useful stuff *)

let ignore_error f arg =
  try ignore (f arg) with _ -> ()

(** ccb : existential type for a (call + callback) type.

    Reference: http://alan.petitepomme.net/cwn/2004.01.13.html
    To rewrite someday with GADT. *)

type 'a poly_ccb = 'a Serialize.call * ('a Interface.value-> unit)
type 't scoped_ccb = { bind_ccb : 'a. 'a poly_ccb -> 't }
type ccb = { open_ccb : 't. 't scoped_ccb -> 't }

let mk_ccb poly = { open_ccb = fun scope -> scope.bind_ccb poly }
let with_ccb ccb e = ccb.open_ccb e

(** * The structure describing a coqtop sub-process *)

type logger = Interface.message_level -> string -> unit

type handle = {
  pid : int; (* Unix process id *)
  cout : Unix.file_descr;
  cin : out_channel;
  mutable alive : bool;
  mutable waiting_for : (ccb * logger) option; (* last call + callback + log *)
  mutable io_watch_id : Glib.Io.id option; (* for removing the gtk io watch *)
}

type coqtop = {
  (* non quoted command-line arguments of coqtop *)
  sup_args : string list;
  (* actual coqtop process *)
  mutable handle : handle;
  (* trigger called whenever coqtop dies abruptly *)
  trigger : handle -> unit;
  (* whether coqtop may be relaunched *)
  mutable is_closed : bool;
  (* whether coqtop is busy *)
  mutable is_computing : bool;
  (* whether coqtop is waiting to be resetted *)
  mutable is_to_reset : bool;
  (* whether coqtop has received its first initialization commands *)
  mutable is_initialized : bool;
}

(** Invariants:
  - any outside request takes the coqtop.lock and is discarded when
    [is_closed = true].
  - coqtop.handle may be written ONLY when toplvl_ctr_mtx AND coqtop.lock is 
    taken.
*)

exception DeadCoqtop

(** * Count of all active coqtops *)

let toplvl_ctr = ref 0

let coqtop_zombies () = !toplvl_ctr

(** * Starting / signaling / ending a real coqtop sub-process *)

(** We simulate a Unix.open_process that also returns the pid of
    the created process. Note: this uses Unix.create_process, which
    doesn't call bin/sh, so args shouldn't be quoted. The process
    cannot be terminated by a Unix.close_process, but rather by a
    kill of the pid.

           >--ide2top_w--[pipe]--ide2top_r-->
    coqide                                   coqtop
           <--top2ide_r--[pipe]--top2ide_w--<

    Note: we use Unix.stderr in Unix.create_process to get debug
    messages from the coqtop's Ide_slave loop.

    NB: it's important to close coqide's descriptors (ide2top_w and top2ide_r)
    in coqtop. We do this indirectly via [Unix.set_close_on_exec].
    This way, coqide has the only remaining copies of these descriptors,
    and closing them later will have visible effects in coqtop. Cf man 7 pipe :

    - If  all file descriptors referring to the write end of a pipe have been
      closed, then an attempt to read(2) from the pipe will see end-of-file
      (read(2) will return 0).
    - If all file descriptors referring to the read end of a pipe have been
      closed, then a write(2) will cause a SIGPIPE signal to be generated for
      the calling process. If the calling process is ignoring this signal,
      then write(2) fails with the error EPIPE.

    Symmetrically, coqtop's descriptors (ide2top_r and top2ide_w) should be
    closed in coqide.
*)

let open_process_pid prog args =
  let (ide2top_r,ide2top_w) = Unix.pipe () in
  let (top2ide_r,top2ide_w) = Unix.pipe () in
  Unix.set_close_on_exec ide2top_w;
  Unix.set_close_on_exec top2ide_r;
  let pid = Unix.create_process prog args ide2top_r top2ide_w Unix.stderr in
  assert (pid <> 0);
  Unix.close ide2top_r;
  Unix.close top2ide_w;
  (pid,top2ide_r,Unix.out_channel_of_descr ide2top_w)

let maxread = 1024

let read_string = String.make maxread ' '
let read_buffer = Buffer.create maxread

let io_read_all chan =
  Buffer.clear read_buffer;
  let rec loop () =
    let len = Glib.Io.read ~buf:read_string ~pos:0 ~len:maxread chan in
    Buffer.add_substring read_buffer read_string 0 len;
    if len < maxread then Buffer.contents read_buffer
    else loop ()
  in loop ()

exception TubeError
exception AnswerWithoutRequest

let install_input_watch handle =
  let io_chan = Glib.Io.channel_of_descr handle.cout in
  let all_conds = [`ERR; `HUP; `IN; `NVAL; `PRI] in (* all except `OUT *)
  let rec check_errors = function
    | [] -> ()
    | (`IN | `PRI) :: conds -> check_errors conds
    | e :: _ -> raise TubeError
  in
  let handle_intermediate_message logger xml =
    let message = Serialize.to_message xml in
    let level = message.Interface.message_level in
    let content = message.Interface.message_content in
    logger level content
  in
  let handle_final_answer ccb xml =
    Minilib.log "END Wait";
    handle.waiting_for <- None;
    with_ccb ccb { bind_ccb = fun (c,f) -> f (Serialize.to_answer xml c) }
  in
  let unsafe_handle_input conds =
    check_errors conds;
    let s = io_read_all io_chan in
    let len = String.length s in
    if s = "" then raise TubeError;
    match handle.waiting_for with
      |None -> raise AnswerWithoutRequest
      |Some (ccb,logger) ->
	let lex = Lexing.from_string s in
	let p = Xml_parser.make (Xml_parser.SLexbuf lex) in
	Xml_parser.check_eof p false;
	let rec loop () =
	  if Lexing.lexeme_end lex = len then (* EOF *) ()
	  else
	    let xml = Xml_parser.parse p in
	    if Serialize.is_message xml then
	      (handle_intermediate_message logger xml; loop ())
	    else
	      handle_final_answer ccb xml
	in
	loop (); true
  in
  let print_exception = function
    | Xml_parser.Error e -> Xml_parser.error e
    | Serialize.Marshal_error -> "Protocol violation"
    | e -> Printexc.to_string e
  in
  let handle_input conds =
    try unsafe_handle_input conds
    with e ->
      Minilib.log ("Exception in coqtop reader: "^print_exception e);
      (* TODOPL : trigger a reset *)
      false
  in
  handle.io_watch_id <-
    (Some (Glib.Io.add_watch ~cond:all_conds ~callback:handle_input io_chan))

(* TODOPL : remove the handler someday *)

(** This launches a fresh handle from its command line arguments. *)
let unsafe_spawn_handle args =
  let prog = coqtop_path () in
  let args = Array.of_list (prog :: "-ideslave" :: args) in
  let (pid, in_fd, oc) = open_process_pid prog args in
  incr toplvl_ctr;
  let h = {
    pid = pid;
    cin = oc;
    cout = in_fd;
    alive = true;
    waiting_for = None;
    io_watch_id = None;
  }
  in
  install_input_watch h;
  h

(** This clears any potentially remaining open garbage. *)
let unsafe_clear_handle coqtop =
  let handle = coqtop.handle in
  if handle.alive then begin
    (* invalidate the old handle *)
    handle.alive <- false;
    ignore_error close_out handle.cin;
    ignore_error Unix.close handle.cout;
    ignore_error (Unix.waitpid []) handle.pid;
    decr toplvl_ctr
  end

let spawn_coqtop hook sup_args =
  {
    handle = unsafe_spawn_handle sup_args;
    sup_args = sup_args;
    trigger = hook;
    is_closed = false;
    is_computing = false;
    is_to_reset = false;
    is_initialized = false;
  }

let interrupter = ref (fun pid -> Unix.kill pid Sys.sigint)
let killer = ref (fun pid -> Unix.kill pid Sys.sigkill)

let is_computing coqtop = coqtop.is_computing

let is_closed coqtop = coqtop.is_closed

(** These are asynchronous signals *)
let break_coqtop coqtop =
  try !interrupter coqtop.handle.pid
  with _ -> Minilib.log "Error while sending Ctrl-C"

let kill_coqtop coqtop =
  try !killer coqtop.handle.pid
  with _ -> Minilib.log "Kill -9 failed. Process already terminated ?"

type task = handle -> (unit -> unit) -> unit

let unsafe_process coqtop task =
  coqtop.is_computing <- true;
  try
    task coqtop.handle
      (fun () ->
	coqtop.is_computing <- false;
	coqtop.is_initialized <- true)
  with
  | DeadCoqtop ->
    coqtop.is_computing <- false;
    (* Coqtop died from an non natural cause, let's try to relaunch it *)
    if not coqtop.is_closed && not coqtop.is_to_reset then begin
      ignore_error unsafe_clear_handle coqtop;
      let try_respawn () =
	coqtop.handle <- unsafe_spawn_handle coqtop.sup_args;
      in
      ignore_error try_respawn ();
      (* If respawning coqtop failed, there is not much we can do... *)
      assert (coqtop.handle.alive = true);
      (* Process the reset callback *)
      ignore_error coqtop.trigger coqtop.handle;
    end
  | err ->
    (* Another error occured, we propagate it. *)
    coqtop.is_computing <- false;
    raise err

let try_grab coqtop task abort =
  if coqtop.is_computing || not coqtop.is_initialized then abort ()
  else if coqtop.is_closed || coqtop.is_to_reset then ()
  else unsafe_process coqtop task

let init_coqtop coqtop task =
  assert (coqtop.is_initialized = false);
  if coqtop.is_closed || coqtop.is_to_reset then ()
  else unsafe_process coqtop task

(** * Calls to coqtop *)

(** Cf [Ide_intf] for more details *)

type 'a atask = handle -> ('a Interface.value -> unit) -> unit

let eval_call ?(logger=default_logger) call handle k =
  (** Send messages to coqtop and prepare the decoding of the answer *)
  try
    Minilib.log ("Start of eval_call " ^ Serialize.pr_call call);
    assert (handle.waiting_for = None); (* TODOPL !! *)
    Minilib.log "START Wait";
    handle.waiting_for <- Some (mk_ccb (call,k), logger);
    Xml_utils.print_xml handle.cin (Serialize.of_call call);
    flush handle.cin;
    Minilib.log "End of eval_call";
  with err ->
    (* if anything else happens here, coqtop is most likely dead *)
    let msg = Printf.sprintf "Error communicating with pid [%i]: %s"
      handle.pid (Printexc.to_string err)
    in
    Minilib.log msg;
    raise DeadCoqtop

let interp ?(logger=default_logger) ?(raw=false) ?(verbose=true) s =
  eval_call ~logger (Serialize.interp (raw,verbose,s))
let rewind i = eval_call (Serialize.rewind i)
let inloadpath s = eval_call (Serialize.inloadpath s)
let mkcases s = eval_call (Serialize.mkcases s)
let status = eval_call Serialize.status
let hints = eval_call Serialize.hints
let search flags = eval_call (Serialize.search flags)

let unsafe_close coqtop =
  if not (coqtop.is_computing) then
    try
      eval_call Serialize.quit coqtop.handle
	(function Interface.Good _ -> () | _ -> kill_coqtop coqtop)
    with _ -> kill_coqtop coqtop
  else
    (* bring me the chainsaw! *)
    kill_coqtop coqtop

let close_coqtop coqtop =
  coqtop.is_closed <- true;
  (* try to quit coqtop gently *)
  unsafe_close coqtop;
  (* Finalize the handle *)
  ignore_error unsafe_clear_handle coqtop

let reset_coqtop coqtop hook =
  coqtop.is_to_reset <- true;
  (* try to quit coqtop gently *)
  unsafe_close coqtop;
  (* Reset the handle *)
  ignore_error unsafe_clear_handle coqtop;
  let try_respawn () =
    coqtop.handle <- unsafe_spawn_handle coqtop.sup_args;
  in
  ignore_error try_respawn ();
  (* If respawning coqtop failed, there is not much we can do... *)
  assert (coqtop.handle.alive = true);
  (* Reset done *)
  coqtop.is_to_reset <- false;
  (* Process the reset callback with the given hook *)
  ignore_error (hook coqtop.handle) (fun () -> coqtop.is_initialized <- true)

module PrintOpt =
struct
  type t = string list

  let width_ref = ref None
  let set_printing_width w = width_ref := Some w

  let width = ["Printing"; "Width"]
  let implicit = ["Printing"; "Implicit"]
  let coercions = ["Printing"; "Coercions"]
  let raw_matching = ["Printing"; "Matching"]
  let notations = ["Printing"; "Notations"]
  let all_basic = ["Printing"; "All"]
  let existential = ["Printing"; "Existential"; "Instances"]
  let universes = ["Printing"; "Universes"]

  let state_hack = Hashtbl.create 11
  let _ = List.iter (fun opt -> Hashtbl.add state_hack opt false)
            [ implicit; coercions; raw_matching; notations;
	      all_basic; existential; universes ]

  let set opts h k =
    List.iter (fun (name, v) -> Hashtbl.replace state_hack name v) opts;
    let opts = List.map (fun (n, v) -> (n, Interface.BoolValue v)) opts in
    let opts = (width, Interface.IntValue !width_ref):: opts in
    eval_call (Serialize.set_options opts) h
      (function
	| Interface.Good () -> k ()
	| _ -> (* TODOPL *)
	  raise (Failure "Cannot set options."))

  let enforce_hack h k =
    let elements = Hashtbl.fold (fun opt v acc -> (opt, v) :: acc) state_hack []
    in
    set elements h k

end

let goals h k =
  PrintOpt.enforce_hack h (fun () -> eval_call Serialize.goals h k)

let evars h k =
  PrintOpt.enforce_hack h (fun () -> eval_call Serialize.evars h k)
