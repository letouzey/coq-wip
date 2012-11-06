(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Preferences
open Gtk_parsing
open Ideutils

type flag = [ `COMMENT | `UNSAFE ]

type ide_info = {
  start : GText.mark;
  stop : GText.mark;
  flags : flag list;
}

type direction = Up | Down

class type _analyzed_view =
object

  method filename : string option
  method stats :  Unix.stats option
  method update_stats : unit
  method revert : unit
  method auto_save : unit
  method save : string -> bool
  method save_as : string -> bool
  method get_insert : GText.iter
  method recenter_insert : unit
  method get_start_of_input : GText.iter
  method insert_message : string -> unit
  method set_message : string -> unit
  method find_next_occurrence : direction -> unit
  method help_for_keyword : unit -> unit

  method go_to_insert : Coq.task
  method tactic_wizard : string list -> Coq.task
  method process_next_phrase : Coq.task
  method process_until_end_or_error : Coq.task
  method erroneous_reset_initial : Coq.task
  method requested_reset_initial : Coq.task
  method raw_coq_query : string -> Coq.task
  method show_goals : Coq.task
  method backtrack_last_phrase : Coq.task
  method include_file_dir_in_path : Coq.task
end


type viewable_script = {
  script : Wg_ScriptView.script_view;
  tab_label : GMisc.label;
  proof_view : Wg_ProofView.proof_view;
  message_view : Wg_MessageView.message_view;
  analyzed_view : _analyzed_view;
  toplvl : Coq.coqtop;
  command : Wg_Command.command_window;
  finder : Wg_Find.finder;
}

let kill_session s =
  (* To close the detached views of this script, we call manually
     [destroy] on it, triggering some callbacks in [detach_view].
     In a more modern lablgtk, rather use the page-removed signal ? *)
  s.script#destroy ();
  Coq.close_coqtop s.toplvl

let build_session s =
  let session_paned = GPack.paned `VERTICAL () in
  let session_box =
    GPack.vbox ~packing:(session_paned#pack1 ~shrink:false ~resize:true) ()
  in
  let eval_paned = GPack.paned `HORIZONTAL ~border_width:5
    ~packing:(session_box#pack ~expand:true) () in
  let script_frame = GBin.frame ~shadow_type:`IN
    ~packing:eval_paned#add1 () in
  let script_scroll = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC ~packing:script_frame#add () in
  let state_paned = GPack.paned `VERTICAL
    ~packing:eval_paned#add2 () in
  let proof_frame = GBin.frame ~shadow_type:`IN
    ~packing:state_paned#add1 () in
  let proof_scroll = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC ~packing:proof_frame#add () in
  let message_frame = GBin.frame ~shadow_type:`IN
    ~packing:state_paned#add2 () in
  let message_scroll = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC ~packing:message_frame#add () in
  let session_tab = GPack.hbox ~homogeneous:false () in
  let img = GMisc.image ~icon_size:`SMALL_TOOLBAR
    ~packing:session_tab#pack () in
  let _ =
    s.script#buffer#connect#modified_changed
      ~callback:(fun () -> if s.script#buffer#modified
        then img#set_stock `SAVE
        else img#set_stock `YES) in
  let _ =
    eval_paned#misc#connect#size_allocate
      ~callback:
      (let old_paned_width = ref 2 in
       let old_paned_height = ref 2 in
       fun {Gtk.width=paned_width;Gtk.height=paned_height} ->
	 if !old_paned_width <> paned_width ||
	    !old_paned_height <> paned_height
	 then begin
	   eval_paned#set_position
	     (eval_paned#position * paned_width / !old_paned_width);
	   state_paned#set_position
	     (state_paned#position * paned_height / !old_paned_height);
	   old_paned_width := paned_width;
	   old_paned_height := paned_height;
	 end)
  in
  session_box#pack s.finder#coerce;
  session_paned#pack2 ~shrink:false ~resize:false (s.command#frame#coerce);
  script_scroll#add s.script#coerce;
  proof_scroll#add s.proof_view#coerce;
  message_scroll#add s.message_view#coerce;
  session_tab#pack s.tab_label#coerce;
  img#set_stock `YES;
  eval_paned#set_position 1;
  state_paned#set_position 1;
  (Some session_tab#coerce,None,session_paned#coerce)

let session_notebook =
  Wg_Notebook.create build_session kill_session
    ~border_width:2 ~show_border:false ~scrollable:true ()

let cb = GData.clipboard Gdk.Atom.primary

let update_notebook_pos () =
  let pos =
    match current.vertical_tabs, current.opposite_tabs with
      | false, false -> `TOP
      | false, true  -> `BOTTOM
      | true , false -> `LEFT
      | true , true  -> `RIGHT
  in
  session_notebook#set_tab_pos pos

(** * Coqide's handling of signals *)

(** We ignore Ctrl-C, and for most of the other catchable signals
    we launch an emergency save of opened files and then exit *)

let signals_to_crash = [Sys.sigabrt; Sys.sigalrm; Sys.sigfpe; Sys.sighup;
			Sys.sigill; Sys.sigpipe; Sys.sigquit;
			(* Sys.sigsegv; Sys.sigterm;*) Sys.sigusr2]

let crash_save i =
  (*  ignore (Unix.sigprocmask Unix.SIG_BLOCK signals_to_crash);*)
  Minilib.log "Trying to save all buffers in .crashcoqide files";
  let count = ref 0 in
  List.iter
    (function {script=view; analyzed_view = av } ->
      (let filename = match av#filename with
	| None ->
	  incr count;
	  "Unnamed_coqscript_"^(string_of_int !count)^".crashcoqide"
	| Some f -> f^".crashcoqide"
       in
       try
	 if try_export filename (view#buffer#get_text ()) then
	   Minilib.log ("Saved "^filename)
	 else Minilib.log ("Could not save "^filename)
       with _ -> Minilib.log ("Could not save "^filename))
    )
    session_notebook#pages;
  Minilib.log "Done. Please report.";
  if i <> 127 then exit i

let ignore_break () =
  List.iter
    (fun i ->
      try Sys.set_signal i (Sys.Signal_handle crash_save)
      with _ -> Minilib.log "Signal ignored (normal if Win32)")
    signals_to_crash;
  Sys.set_signal Sys.sigint Sys.Signal_ignore


exception Unsuccessful

let force_reset_initial () =
  Minilib.log "Reset Initial";
  let term = session_notebook#current_term in
  Coq.reset_coqtop term.toplvl term.analyzed_view#requested_reset_initial

let break () =
  Minilib.log "User break received";
  Coq.break_coqtop session_notebook#current_term.toplvl

let warn_image =
  let img = GMisc.image () in
  img#set_stock `DIALOG_WARNING;
  img#set_icon_size `DIALOG;
  img#coerce

let warning msg =
  GToolbox.message_box ~title:"Warning" ~icon:warn_image msg

module Opt = Coq.PrintOpt

let print_items = [
  ([Opt.implicit],"Display implicit arguments","Display _implicit arguments",
   "i",false);
  ([Opt.coercions],"Display coercions","Display _coercions","c",false);
  ([Opt.raw_matching],"Display raw matching expressions",
   "Display raw _matching expressions","m",true);
  ([Opt.notations],"Display notations","Display _notations","n",true);
  ([Opt.all_basic],"Display all basic low-level contents",
   "Display _all basic low-level contents","a",false);
  ([Opt.existential],"Display existential variable instances",
   "Display _existential variable instances","e",false);
  ([Opt.universes],"Display universe levels","Display _universe levels",
   "u",false);
  ([Opt.all_basic;Opt.existential;Opt.universes],
   "Display all low-level contents", "Display all _low-level contents",
   "l",false)
]

let get_current_word () =
  match session_notebook#current_term,cb#text with
    | {script=script; analyzed_view=av;},None ->
      Minilib.log "None selected";
      let it = av#get_insert in
      let start = find_word_start it in
      let stop = find_word_end start in
      script#buffer#move_mark `SEL_BOUND ~where:start;
      script#buffer#move_mark `INSERT ~where:stop;
      script#buffer#get_text ~slice:true ~start ~stop ()
    | _,Some t ->
      Minilib.log "Some selected";
      Minilib.log t;
      t

let input_channel b ic =
  let buf = String.create 1024 and len = ref 0 in
  while len := input ic buf 0 1024; !len > 0 do
    Buffer.add_substring b buf 0 !len
  done

let with_file handler name ~f =
  try
    let ic = open_in_gen [Open_rdonly;Open_creat] 0o644 name in
    try f ic; close_in ic with e -> close_in ic; raise e
  with Sys_error s -> handler s

(** Cut a part of the buffer in sentences and tag them.
    May raise [Coq_lex.Unterminated] when the zone ends with
    an unterminated sentence. *)

let split_slice_lax (buffer: GText.buffer) from upto =
  buffer#remove_tag ~start:from ~stop:upto Tags.Script.comment_sentence;
  buffer#remove_tag ~start:from ~stop:upto Tags.Script.sentence;
  let slice = buffer#get_text ~start:from ~stop:upto () in
  let rec split_substring str =
    let off_conv = byte_offset_to_char_offset str in
    let slice_len = String.length str in
    let is_comment,end_off = Coq_lex.delimit_sentence str in
    let start = from#forward_chars (off_conv end_off) in
    let stop = start#forward_char in
    let tag =
      if is_comment then Tags.Script.comment_sentence else Tags.Script.sentence
    in
    buffer#apply_tag ~start ~stop tag;
    let next = end_off + 1 in
    if next < slice_len then begin
      ignore (from#nocopy#forward_chars (off_conv next));
      split_substring (String.sub str next (slice_len - next))
    end
  in
  split_substring slice

(** Searching forward and backward a position fulfilling some condition *)

let rec forward_search cond (iter:GText.iter) =
  if iter#is_end || cond iter then iter
  else forward_search cond iter#forward_char

let rec backward_search cond (iter:GText.iter) =
  if iter#is_start || cond iter then iter
  else backward_search cond iter#backward_char

let is_sentence_end s =
  s#has_tag Tags.Script.sentence || s#has_tag Tags.Script.comment_sentence
let is_char s c = s#char = Char.code c

(** Search backward the first character of a sentence, starting at [iter]
    and going at most up to [soi] (meant to be the end of the locked zone).
    Raise [Not_found] when no proper sentence start has been found,
    in particular when the final "." of the locked zone is followed
    by a non-blank character outside the locked zone. This non-blank
    character will be signaled as erroneous in [tag_on_insert] below. *)

let grab_sentence_start (iter:GText.iter) soi =
  let cond iter =
    if iter#compare soi < 0 then raise Not_found;
    let prev = iter#backward_char in
    is_sentence_end prev &&
      (not (is_char prev '.') ||
       List.exists (is_char iter) [' ';'\n';'\r';'\t'])
  in
  backward_search cond iter

(** Search forward the first character immediately after a sentence end *)

let rec grab_sentence_stop (start:GText.iter) =
  (forward_search is_sentence_end start)#forward_char

(** Search forward the first character immediately after a "." sentence end
    (and not just a "{" or "}" or comment end *)

let rec grab_ending_dot (start:GText.iter) =
  let is_ending_dot s = is_sentence_end s && s#char = Char.code '.' in
  (forward_search is_ending_dot start)#forward_char

(** Retag a zone that has been edited *)

let tag_on_insert buffer =
  try
    (* the start of the non-locked zone *)
    let soi = buffer#get_iter_at_mark (`NAME "start_of_input") in
    (* the inserted zone is between [prev_insert] and [insert] *)
    let insert = buffer#get_iter_at_mark `INSERT in
    let prev_insert = buffer#get_iter_at_mark (`NAME "prev_insert") in
    (* [prev_insert] is normally always before [insert] even when deleting.
      Let's check this nonetheless *)
    let prev_insert =
      if insert#compare prev_insert < 0 then insert else prev_insert
    in
    let start = grab_sentence_start prev_insert soi in
    (** The status of "{" "}" as sentence delimiters is too fragile.
        We retag up to the next "." instead. *)
    let stop = grab_ending_dot insert in
    try split_slice_lax buffer start stop
    with Coq_lex.Unterminated ->
      try split_slice_lax buffer start buffer#end_iter
      with Coq_lex.Unterminated -> ()
  with Not_found ->
    (* This is raised by [grab_sentence_start] *)
    let err_pos = buffer#get_iter_at_mark (`NAME "start_of_input") in
    buffer#apply_tag Tags.Script.error
      ~start:err_pos ~stop:err_pos#forward_char

let force_retag buffer =
  try split_slice_lax buffer buffer#start_iter buffer#end_iter
  with Coq_lex.Unterminated -> ()

(* GtkSource view should handle that one day !!!
let toggle_proof_visibility (buffer:GText.buffer) (cursor:GText.iter) =
  (* move back twice if not into proof_decl,
   * once if into proof_decl and back_char into_proof_decl,
   * don't move if into proof_decl and back_char not into proof_decl *)
  if not (cursor#has_tag Tags.Script.proof_decl) then
    ignore (cursor#nocopy#backward_to_tag_toggle (Some Tags.Script.proof_decl));
  if cursor#backward_char#has_tag Tags.Script.proof_decl then
    ignore (cursor#nocopy#backward_to_tag_toggle (Some Tags.Script.proof_decl));
  let decl_start = cursor in
  let prf_end = decl_start#forward_to_tag_toggle (Some Tags.Script.qed) in
  let decl_end = grab_ending_dot decl_start in
  let prf_end = grab_ending_dot prf_end in
  let prf_end = prf_end#forward_char in
  if decl_start#has_tag Tags.Script.folded then (
    buffer#remove_tag ~start:decl_start ~stop:decl_end Tags.Script.folded;
    buffer#remove_tag ~start:decl_end ~stop:prf_end Tags.Script.hidden)
  else (
    buffer#apply_tag ~start:decl_start ~stop:decl_end Tags.Script.folded;
    buffer#apply_tag ~start:decl_end ~stop:prf_end Tags.Script.hidden)
*)

(** The arguments that will be passed to coqtop. No quoting here, since
    no /bin/sh when using create_process instead of open_process. *)
let custom_project_files = ref []
let sup_args = ref []

class analyzed_view (_script:Wg_ScriptView.script_view)
  (_pv:Wg_ProofView.proof_view) (_mv:Wg_MessageView.message_view) _ct _fn =
object(self)
  val input_view = _script
  val input_buffer = _script#source_buffer
  val proof_view = _pv
  val message_view = _mv
  val cmd_stack = Stack.create ()
  val mycoqtop = _ct

  val mutable filename = _fn
  val mutable stats = None
  val mutable last_modification_time = 0.
  val mutable last_auto_save_time = 0.

  val hidden_proofs = Hashtbl.create 32

  method filename = filename
  method stats = stats
  method update_stats =
    match filename with
      | Some f -> stats <- my_stat f
      | _ -> ()

  method revert =
    match filename with
      | None -> ()
      | Some f -> begin
        let do_revert () = begin
          push_info "Reverting buffer";
          try
            Coq.reset_coqtop mycoqtop self#requested_reset_initial;
            let b = Buffer.create 1024 in
            with_file flash_info f ~f:(input_channel b);
            let s = try_convert (Buffer.contents b) in
            input_buffer#set_text s;
            self#update_stats;
            input_buffer#place_cursor ~where:input_buffer#start_iter;
            input_buffer#set_modified false;
            pop_info ();
            flash_info "Buffer reverted";
            force_retag (input_buffer :> GText.buffer);
          with _  ->
            pop_info ();
            flash_info "Warning: could not revert buffer";
        end
        in
        if input_buffer#modified then
          match (GToolbox.question_box
                   ~title:"Modified buffer changed on disk"
                   ~buttons:["Revert from File";
                             "Overwrite File";
                             "Disable Auto Revert"]
                   ~default:0
                   ~icon:(stock_to_widget `DIALOG_WARNING)
                   "Some unsaved buffers changed on disk"
          )
          with 1 -> do_revert ()
            | 2 -> if self#save f then flash_info "Overwritten" else
                flash_info "Could not overwrite file"
            | _ ->
              Minilib.log "Auto revert set to false";
              current.global_auto_revert <- false;
              disconnect_revert_timer ()
        else do_revert ()
      end

  method save f =
    if try_export f (input_buffer#get_text ()) then begin
      filename <- Some f;
      input_buffer#set_modified false;
      stats <- my_stat f;
      (match self#auto_save_name with
        | None -> ()
        | Some fn -> try Sys.remove fn with _ -> ());
      true
    end
    else false

  method private auto_save_name =
    match filename with
      | None -> None
      | Some f ->
        let dir = Filename.dirname f in
        let base = (fst current.auto_save_name) ^
          (Filename.basename f) ^
          (snd current.auto_save_name)
        in Some (Filename.concat dir base)

  method private need_auto_save =
    input_buffer#modified &&
      last_modification_time > last_auto_save_time

  method auto_save =
    if self#need_auto_save then begin
      match self#auto_save_name with
        | None -> ()
        | Some fn ->
          try
            last_auto_save_time <- Unix.time();
            Minilib.log ("Autosave time: "^(string_of_float (Unix.time())));
            if try_export fn (input_buffer#get_text ()) then begin
              flash_info ~delay:1000 "Autosaved"
            end
            else warning
              ("Autosave failed (check if " ^ fn ^ " is writable)")
          with _ ->
            warning ("Autosave: unexpected error while writing "^fn)
    end

  method save_as f =
    if not (Sys.file_exists f) then self#save f
    else
      let res = GToolbox.question_box
	~title:"File exists on disk"
        ~buttons:["Overwrite"; "Cancel";]
        ~default:1
        ~icon:warn_image
        ("File "^f^" already exists")
      in
      match res with
	| 1 -> self#save f
        | _ -> false

  method insert_message s =
    message_view#push Interface.Notice s

  method set_message s =
    message_view#clear ();
    message_view#push Interface.Notice s

  method private push_message level content =
    message_view#push level content

  method get_start_of_input =
    input_buffer#get_iter_at_mark (`NAME "start_of_input")

  method get_insert = get_insert input_buffer

  method recenter_insert =
    input_view#scroll_to_mark
      ~use_align:false
      ~yalign:0.75
      ~within_margin:0.25
      `INSERT

  (* go to the next occurrence of the current word, forward or backward *)
  method find_next_occurrence dir =
    let b = input_buffer in
    let start = find_word_start (self#get_insert) in
    let stop = find_word_end start in
    let text = b#get_text ~start ~stop () in
    let search =
      if dir=Down then stop#forward_search else start#backward_search
    in
    match search text with
      | None -> ()
      | Some(start, _) ->
        (b#place_cursor start;
         self#recenter_insert)

  method show_goals h k =
    Coq.PrintOpt.set_printing_width proof_view#width;
    Coq.goals h (function
      |Interface.Fail (l, str) ->
	(self#set_message ("Error in coqtop:\n"^str); k())
      |Interface.Good goals | Interface.Unsafe goals ->
	Coq.evars h (function
	  |Interface.Fail (l, str)->
	    (self#set_message ("Error in coqtop:\n"^str); k())
	  |Interface.Good evs  | Interface.Unsafe evs ->
	    proof_view#set_goals goals;
	    proof_view#set_evars evs;
	    proof_view#refresh ();
	    k()))

  (* This method is intended to perform stateless commands *)
  method raw_coq_query phrase h k =
    let () = Minilib.log "raw_coq_query starting now" in
    let display_error s =
      if not (Glib.Utf8.validate s) then
        flash_info "This error is so nasty that I can't even display it."
      else self#insert_message s;
    in
    Coq.interp ~logger:self#push_message ~raw:true ~verbose:false phrase h
      (function
	| Interface.Fail (_, err) -> display_error err; k ()
	| Interface.Good msg | Interface.Unsafe msg ->
	  self#insert_message msg; k ())

  method private find_phrase_starting_at (start:GText.iter) =
    try
      let start = grab_sentence_start start self#get_start_of_input in
      let stop = grab_sentence_stop start in
      (* Is this phrase non-empty and complete ? *)
      if stop#compare start > 0 && is_sentence_end stop#backward_char
      then Some (start,stop)
      else None
    with Not_found -> None

  (** [fill_command_queue until q] fills a command queue until the [until]
      condition returns true; it is fed with the number of phrases read and the
      iters enclosing the current sentence. *)
  method private fill_command_queue until queue =
    let rec loop len iter =
      let opt_sentence = self#find_phrase_starting_at iter in
      match opt_sentence with
      | None -> raise Exit
      | Some (start, stop) ->
        if until len start stop then raise Exit;
        input_buffer#apply_tag Tags.Script.to_process ~start ~stop;
        (* Check if this is a comment *)
        let is_comment =
	  stop#backward_char#has_tag Tags.Script.comment_sentence
	in
        let payload = {
          start = `MARK (input_buffer#create_mark start);
          stop = `MARK (input_buffer#create_mark stop);
          flags = if is_comment then [`COMMENT] else [];
        } in
        Queue.push payload queue;
        if not stop#is_end then loop (succ len) stop
    in
    try loop 0 self#get_start_of_input with Exit -> ()

  method private discard_command_queue queue =
    while not (Queue.is_empty queue) do
      let sentence = Queue.pop queue in
      let start = input_buffer#get_iter_at_mark sentence.start in
      let stop = input_buffer#get_iter_at_mark sentence.stop in
      input_buffer#remove_tag Tags.Script.to_process ~start ~stop;
      input_buffer#delete_mark sentence.start;
      input_buffer#delete_mark sentence.stop;
    done

  method private commit_queue_transaction queue sentence newflags =
    (* A queued command has been successfully done, we push it to [cmd_stack].
       We reget the iters here because Gtk is unable to warranty that they
       were not modified meanwhile. Not really necessary but who knows... *)
    let start = input_buffer#get_iter_at_mark sentence.start in
    let stop = input_buffer#get_iter_at_mark sentence.stop in
    let sentence = { sentence with flags = newflags @ sentence.flags } in
    let tag =
      if List.mem `UNSAFE newflags then Tags.Script.unjustified
      else Tags.Script.processed
    in
    input_buffer#move_mark ~where:stop (`NAME "start_of_input");
    input_buffer#apply_tag tag ~start ~stop;
    input_buffer#remove_tag Tags.Script.to_process ~start ~stop;
    ignore (Queue.pop queue);
    Stack.push sentence cmd_stack

  method private process_error queue phrase loc msg h k =
    let position_error = function
      | None -> ()
      | Some (start, stop) ->
	let soi = self#get_start_of_input in
	let start =
	  soi#forward_chars (byte_offset_to_char_offset phrase start) in
	let stop =
	  soi#forward_chars (byte_offset_to_char_offset phrase stop) in
	input_buffer#apply_tag Tags.Script.error ~start ~stop;
	input_buffer#place_cursor ~where:start
    in
    self#discard_command_queue queue;
    pop_info ();
    position_error loc;
    message_view#clear ();
    message_view#push Interface.Error msg;
    self#show_goals h k

  (** Compute the phrases until [until] returns [true]. *)
  method private process_until until verbose h k =
    let queue = Queue.create () in
    (* Lock everything and fill the waiting queue *)
    push_info "Coq is computing";
    message_view#clear ();
    input_view#set_editable false;
    self#fill_command_queue until queue;
    (* Now unlock and process asynchronously *)
    input_view#set_editable true;
    let push_info lvl msg = if verbose then self#push_message lvl msg
    in
    Minilib.log "Begin command processing";
    let rec loop () =
      if Queue.is_empty queue then begin
	pop_info ();
	self#recenter_insert;
	self#show_goals h k
      end else
	let sentence = Queue.peek queue in
	if List.mem `COMMENT sentence.flags then
	  (self#commit_queue_transaction queue sentence []; loop ())
	else
	  (* If the line is not a comment, we interpret it. *)
          let start = input_buffer#get_iter_at_mark sentence.start in
          let stop = input_buffer#get_iter_at_mark sentence.stop in
          let phrase = start#get_slice ~stop in
	  let commit_and_continue msg flags =
	    push_info Interface.Notice msg;
	    self#commit_queue_transaction queue sentence flags;
	    loop ()
	  in
          Coq.interp ~logger:push_info ~verbose phrase h
	    (function
	      |Interface.Good msg -> commit_and_continue msg []
	      |Interface.Unsafe msg -> commit_and_continue msg [`UNSAFE]
	      |Interface.Fail (loc, msg) ->
		self#process_error queue phrase loc msg h k)
    in
    loop ()

  method process_next_phrase h k =
    let until len start stop = 1 <= len in
    self#process_until until true h
      (fun () -> input_buffer#place_cursor self#get_start_of_input; k())

  method private process_until_iter iter h k =
    let until len start stop =
      if current.stop_before then stop#compare iter > 0
      else start#compare iter >= 0
    in
    self#process_until until false h k

  method process_until_end_or_error h k =
    self#process_until_iter input_buffer#end_iter h k

  (** Clear the command stack until [until] returns [true]. Returns the number
      of commands sent to Coqtop to backtrack. *)
  method private clear_command_stack until =
    let rec loop len real_len =
      if Stack.is_empty cmd_stack then real_len
      else
        let phrase = Stack.top cmd_stack in
        let is_comment = List.mem `COMMENT phrase.flags in
        let start = input_buffer#get_iter_at_mark phrase.start in
        let stop = input_buffer#get_iter_at_mark phrase.stop in
        if not (until len real_len start stop) then begin
          (* [until] has not been reached, so we clear this command *)
          ignore (Stack.pop cmd_stack);
          input_buffer#remove_tag Tags.Script.processed ~start ~stop;
          input_buffer#remove_tag Tags.Script.unjustified ~start ~stop;
          input_buffer#move_mark ~where:start (`NAME "start_of_input");
          input_buffer#delete_mark phrase.start;
          input_buffer#delete_mark phrase.stop;
          loop (succ len) (if is_comment then real_len else succ real_len)
        end else
          real_len
    in
    loop 0 0

  (** Actually performs the undoing *)
  method private undo_command_stack n h k =
    Coq.rewind n h (function
      | Interface.Good n | Interface.Unsafe n ->
	let until _ len _ _ = n <= len in
	(* Coqtop requested [n] more ACTUAL backtrack *)
	ignore (self#clear_command_stack until);
	k ()
      | Interface.Fail (l, str) ->
	self#set_message
        ("Error while backtracking: " ^ str ^
         "\nCoqIDE and coqtop may be out of sync, you may want to use Restart.");
	k ())

  (** Wrapper around the raw undo command *)
  method private backtrack_until until h k =
    push_info "Coq is undoing";
    message_view#clear ();
    (* Lock everything *)
    input_view#set_editable false;
    let to_undo = self#clear_command_stack until in
    self#undo_command_stack to_undo h
      (fun () ->
	input_view#set_editable true;
	pop_info ();
	k ())

  method private backtrack_to_iter iter h k =
    let until _ _ _ stop = iter#compare stop >= 0 in
    self#backtrack_until until h
      (* We may have backtracked too much: let's replay *)
      (fun () -> self#process_until_iter iter h k)

  method backtrack_last_phrase h k =
    let until len _ _ _ = 1 <= len in
    self#backtrack_until until h
      (fun () ->
	input_buffer#place_cursor self#get_start_of_input;
	self#show_goals h k)

  method go_to_insert h k =
    let point = self#get_insert in
    if point#compare self#get_start_of_input >= 0
    then self#process_until_iter point h k
    else self#backtrack_to_iter point h k

  method tactic_wizard l h k =
    let insert_phrase phrase tag =
      let stop = self#get_start_of_input in
      if stop#starts_line then
        input_buffer#insert ~iter:stop phrase
      else input_buffer#insert ~iter:stop ("\n"^phrase);
      tag_on_insert (input_buffer :> GText.buffer);
      let start = self#get_start_of_input in
      input_buffer#move_mark ~where:stop (`NAME "start_of_input");
      input_buffer#apply_tag tag ~start ~stop;
      if self#get_insert#compare stop <= 0 then
        input_buffer#place_cursor ~where:stop;
      let ide_payload = {
        start = `MARK (input_buffer#create_mark start);
        stop = `MARK (input_buffer#create_mark stop);
        flags = [];
      } in
      Stack.push ide_payload cmd_stack;
      message_view#clear ();
      self#show_goals h k;
    in
    let display_error (loc, s) =
      if not (Glib.Utf8.validate s) then
        flash_info "This error is so nasty that I can't even display it."
      else self#insert_message s
    in
    let try_phrase phrase stop more =
      Minilib.log "Sending to coq now";
      Coq.interp ~verbose:false phrase h
	(function
	  |Interface.Fail (l, str) ->
	    display_error (l, str);
	    self#insert_message ("Unsuccessfully tried: "^phrase);
	    more ()
	  |Interface.Good msg ->
	    self#insert_message msg;
	    stop Tags.Script.processed
	  |Interface.Unsafe msg ->
	    self#insert_message msg;
	    stop Tags.Script.unjustified)
    in
    let rec loop l () = match l with
      | [] -> k ()
      | p :: l' ->
        try_phrase ("progress "^p^".") (insert_phrase (p^".")) (loop l')
    in
    loop l ()

  method private generic_reset_initial h k =
    let start = input_buffer#start_iter in
    (* clear the stack *)
    while not (Stack.is_empty cmd_stack) do
      let phrase = Stack.pop cmd_stack in
      input_buffer#delete_mark phrase.start;
      input_buffer#delete_mark phrase.stop
    done;
    (* reset the buffer *)
    input_buffer#move_mark ~where:start (`NAME "start_of_input");
    input_buffer#remove_tag Tags.Script.processed start input_buffer#end_iter;
    input_buffer#remove_tag Tags.Script.unjustified start input_buffer#end_iter;
    input_buffer#remove_tag Tags.Script.to_process start input_buffer#end_iter;
    tag_on_insert (input_buffer :> GText.buffer);
    (* clear the views *)
    message_view#clear ();
    proof_view#clear ();
    (* apply the initial commands to coq *)
    self#include_file_dir_in_path h k

  method erroneous_reset_initial h k =
    self#generic_reset_initial h
      (fun () ->
	(* warn the user with a pop-up *)
	warning "Coqtop died badly. Resetting.";
	k ())

  method requested_reset_initial h k =
    self#generic_reset_initial h k

  method include_file_dir_in_path h k =
    match filename with
      |None -> k ()
      |Some f ->
	let dir = Filename.dirname f in
	Coq.inloadpath dir h (function
	  |Interface.Fail (_,s) ->
	    self#set_message
	      ("Could not determine lodpath, this might lead to problems:\n"^s);
	    k ()
	  |Interface.Good true |Interface.Unsafe true -> k ()
	  |Interface.Good false |Interface.Unsafe false ->
	    let cmd = Printf.sprintf "Add LoadPath \"%s\". "  dir in
	    Coq.interp cmd h (function
	      |Interface.Fail (l,str) ->
		self#set_message ("Couln't add loadpath:\n"^str);
		k ()
	      |Interface.Good _ | Interface.Unsafe _ -> k ()))

  method help_for_keyword () =
    browse_keyword (self#insert_message) (get_current_word ())

(** NB: Events during text edition:

    - [begin_user_action]
    - [insert_text] (or [delete_range] when deleting)
    - [changed]
    - [end_user_action]

   When pasting a text containing tags (e.g. the sentence terminators),
   there is actually many [insert_text] and [changed]. For instance,
   for "a. b.":

    - [begin_user_action]
    - [insert_text] (for "a")
    - [changed]
    - [insert_text] (for ".")
    - [changed]
    - [apply_tag] (for the tag of ".")
    - [insert_text] (for " b")
    - [changed]
    - [insert_text] (for ".")
    - [changed]
    - [apply_tag] (for the tag of ".")
    - [end_user_action]

  Since these copy-pasted tags may interact badly with the retag mechanism,
  we now don't monitor the "changed" event, but rather the "begin_user_action"
  and "end_user_action". We begin by setting a mark at the initial cursor
  point. At the end, the zone between the mark and the cursor is to be
  untagged and then retagged. *)

  initializer
  let _ = input_buffer#connect#insert_text
    ~callback:(fun it s ->
      if (it#compare self#get_start_of_input)<0
      then GtkSignal.stop_emit ();
      if String.length s > 1 then begin
        Minilib.log "insert_text: Placing cursor";
	input_buffer#place_cursor ~where:it
      end)
  in
  let _ = input_buffer#connect#after#apply_tag
    ~callback:(fun tag ~start ~stop ->
      if (start#compare self#get_start_of_input)>=0
      then begin
        input_buffer#remove_tag Tags.Script.processed ~start ~stop;
        input_buffer#remove_tag Tags.Script.unjustified ~start ~stop
      end)
  in
  let _ = input_buffer#connect#begin_user_action
    ~callback:(fun () ->
      let here = input_buffer#get_iter_at_mark `INSERT in
      input_buffer#move_mark (`NAME "prev_insert") here)
  in
  let _ = input_buffer#connect#end_user_action
    ~callback:(fun () ->
      last_modification_time <- Unix.time ();
      let r = input_view#visible_rect in
      let stop =
        input_view#get_iter_at_location
          ~x:(Gdk.Rectangle.x r + Gdk.Rectangle.width r)
          ~y:(Gdk.Rectangle.y r + Gdk.Rectangle.height r)
      in
      input_buffer#remove_tag
        Tags.Script.error
        ~start:self#get_start_of_input
        ~stop;
      tag_on_insert (input_buffer :> GText.buffer))
  in
  let _ = input_buffer#add_selection_clipboard cb
  in
  let _ = input_buffer#connect#after#mark_set
    ~callback:(fun it (m:Gtk.text_mark) ->
      !set_location
        (Printf.sprintf
           "Line: %5d Char: %3d" (self#get_insert#line + 1)
           (self#get_insert#line_offset + 1));
      match GtkText.Mark.get_name m  with
        | Some "insert" -> ()
        | Some s ->
          Minilib.log (s^" moved")
        | None -> ())
  in
  let _ = input_buffer#connect#insert_text
      ~callback:(fun it s ->
        Minilib.log "Should recenter ?";
        if String.contains s '\n' then begin
          Minilib.log "Should recenter: yes";
          self#recenter_insert
        end)
  in ()
end

let last_make = ref "";;
let last_make_index = ref 0;;
let search_compile_error_regexp =
  Str.regexp
    "File \"\\([^\"]+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)";;

let search_next_error () =
  let _ =
    Str.search_forward search_compile_error_regexp !last_make !last_make_index
  in
  let f = Str.matched_group 1 !last_make
  and l = int_of_string (Str.matched_group 2 !last_make)
  and b = int_of_string (Str.matched_group 3 !last_make)
  and e = int_of_string (Str.matched_group 4 !last_make)
  and msg_index = Str.match_beginning ()
  in
  last_make_index := Str.group_end 4;
  (f,l,b,e,
   String.sub !last_make msg_index (String.length !last_make - msg_index))



(**********************************************************************)
(* session creation and primitive handling                            *)
(**********************************************************************)

let create_session file =
  let script_buffer =
    GSourceView2.source_buffer
      ~tag_table:Tags.Script.table
      ~highlight_matching_brackets:true
      ?language:(lang_manager#language current.source_language)
      ?style_scheme:(style_manager#style_scheme current.source_style)
      ()
  in
  let proof = Wg_ProofView.proof_view () in
  let message = Wg_MessageView.message_view () in
  let basename = match file with
    |None -> "*scratch*"
    |Some f -> Glib.Convert.filename_to_utf8 (Filename.basename f)
  in
  let coqtop_args = match file with
    |None -> !sup_args
    |Some the_file -> match current.read_project with
	|Ignore_args -> !sup_args
	|Append_args ->
	  (Project_file.args_from_project the_file !custom_project_files
	     current.project_file_name) @ !sup_args
	|Subst_args ->
	  Project_file.args_from_project the_file !custom_project_files
	    current.project_file_name
  in
  let reset = ref (fun _ k -> k ()) in
  let trigger handle = !reset handle in
  let ct = Coq.spawn_coqtop trigger coqtop_args in
  let script =
    Wg_ScriptView.script_view ct
      ~source_buffer:script_buffer
      ~show_line_numbers:true
      ~wrap_mode:`NONE () in
  let command = new Wg_Command.command_window ct in
  let finder = new Wg_Find.finder (script :> GText.view) in
  let legacy_av = new analyzed_view script proof message ct file in
  reset := legacy_av#erroneous_reset_initial;
  legacy_av#update_stats;
  ignore
    (script#buffer#create_mark ~name:"start_of_input" script#buffer#start_iter);
  ignore
    (script#buffer#create_mark ~name:"prev_insert" script#buffer#start_iter);
  GtkBase.Widget.add_events proof#as_widget [`ENTER_NOTIFY;`POINTER_MOTION];
  Coq.init_coqtop ct
    (fun h k ->
      legacy_av#include_file_dir_in_path h
	(fun () ->
	  let fold accu (opts, _, _, _, dflt) =
	    List.fold_left (fun accu opt -> (opt, dflt) :: accu) accu opts
	  in
	  let options = List.fold_left fold [] print_items in
	  Coq.PrintOpt.set options h k));
  script#misc#set_name "ScriptWindow";
  script#buffer#place_cursor ~where:script#buffer#start_iter;
  proof#misc#set_can_focus true;
  message#misc#set_can_focus true;

  { tab_label= GMisc.label ~text:basename ();
    script=script;
    proof_view=proof;
    message_view=message;
    analyzed_view=legacy_av;
    toplvl=ct;
    command=command;
    finder=finder;
  }


(*********************************************************************)
(* functions called by the user interface                            *)
(*********************************************************************)

(* Nota: using && here has the advantage of working both under win32 and unix.
   If someday we want the main command to be tried even if the "cd" has failed,
   then we should use " ; " under unix but " & " under win32 (cf. #2363).
*)

let pr_exit_status = function
  | Unix.WEXITED 0 -> " succeeded"
  | _ -> " failed"

let run_command av cmd =
  CUnix.run_command Ideutils.try_convert av#insert_message cmd

let local_cd file =
  "cd " ^ Filename.quote (Filename.dirname file) ^ " && "

let load_file handler f =
  let f = CUnix.correct_path f (Sys.getcwd ()) in
  try
    Minilib.log "Loading file starts";
    let is_f = CUnix.same_file f in
      if not (Util.List.fold_left_i
		(fun i found x -> if found then found else
                   let {analyzed_view=av} = x in
                     (match av#filename with
			| None -> false
			| Some fn ->
			    if is_f fn
			    then (session_notebook#goto_page i; true)
			    else false))
              0 false session_notebook#pages)
      then begin
	Minilib.log "Loading: must open";
	let b = Buffer.create 1024 in
	Minilib.log "Loading: get raw content";
	with_file handler f ~f:(input_channel b);
	Minilib.log "Loading: convert content";
	let s = do_convert (Buffer.contents b) in
	Minilib.log "Loading: create view";
	let session = create_session (Some f) in
	Minilib.log "Loading: adding view";
	let index = session_notebook#append_term session in
	let av = session.analyzed_view in
	Minilib.log "Loading: stats";
	av#update_stats;
	let input_buffer = session.script#buffer in
	Minilib.log "Loading: fill buffer";
	input_buffer#set_text s;
	input_buffer#place_cursor ~where:input_buffer#start_iter;
	force_retag input_buffer;
	Minilib.log ("Loading: switch to view "^ string_of_int index);
	session_notebook#goto_page index;
	Minilib.log "Loading: highlight";
	input_buffer#set_modified false;
	Minilib.log "Loading: clear undo";
	session.script#clear_undo ();
	Minilib.log "Loading: success";
        !refresh_editor_hook ();
      end
  with
    | e -> handler ("Load failed: "^(Printexc.to_string e))

let do_load file = load_file flash_info file

let current_view () = session_notebook#current_term.analyzed_view

(** Callbacks for the File menu *)

module File = struct

let newfile _ =
  let session = create_session None in
  let index = session_notebook#append_term session in
  !refresh_editor_hook ();
  session_notebook#goto_page index

let load _ =
  match select_file_for_open ~title:"Load file" () with
    | None -> ()
    | Some f -> do_load f

let save _ =
  let current = session_notebook#current_term in
  try
    begin match current.analyzed_view#filename with
      | None ->
        begin match select_file_for_save ~title:"Save file" ()
          with
            | None -> ()
            | Some f ->
              if current.analyzed_view#save_as f then begin
                current.tab_label#set_text (Filename.basename f);
                flash_info ("File " ^ f ^ " saved")
              end
              else warning ("Save Failed (check if " ^ f ^ " is writable)")
        end
      | Some f ->
        if current.analyzed_view#save f then flash_info ("File " ^ f ^ " saved")
        else warning  ("Save failed (is " ^ f ^ " writable ?)")
    end
  with e -> warning "Save: unexpected error while saving file"

let saveas _ =
  let current = session_notebook#current_term in
  try
    begin match current.analyzed_view#filename with
      | None ->
        begin match select_file_for_save ~title:"Save file as" ()
          with
            | None -> ()
            | Some f ->
              if current.analyzed_view#save_as f then begin
                current.tab_label#set_text (Filename.basename f);
                flash_info "Saved"
              end
              else flash_info "Save Failed"
        end
      | Some f ->
        begin match select_file_for_save
            ~dir:(ref (Filename.dirname f))
            ~filename:(Filename.basename f)
            ~title:"Save file as" ()
          with
            | None -> ()
            | Some f ->
              if current.analyzed_view#save_as f then begin
                current.tab_label#set_text (Filename.basename f);
                flash_info "Saved"
              end else flash_info "Save failed"
        end
    end
  with e -> flash_info "Save failed"

let saveall _ =
  List.iter
    (function {analyzed_view = av} -> match av#filename with
      | None -> ()
      | Some f -> ignore (av#save f))
    session_notebook#pages

let revert_all _ =
  let revert_one {analyzed_view = av} =
    try
      match av#filename,av#stats with
	| Some f,Some stats ->
          let new_stats = Unix.stat f in
          if new_stats.Unix.st_mtime > stats.Unix.st_mtime
          then av#revert
	| Some _, None -> av#revert
	| _ -> ()
    with _ -> av#revert
  in
  List.iter revert_one session_notebook#pages

let export kind _ =
  let av = current_view () in
  match av#filename with
    | None ->
      flash_info "Cannot print: this buffer has no name"
    | Some f ->
      let basef = Filename.basename f in
      let output =
        let basef_we = try Filename.chop_extension basef with _ -> basef in
        match kind with
          | "latex" -> basef_we ^ ".tex"
          | "dvi" | "ps" | "pdf" | "html" -> basef_we ^ "." ^ kind
          | _ -> assert false
      in
      let cmd =
        local_cd f ^ current.cmd_coqdoc ^ " --" ^ kind ^
	  " -o " ^ (Filename.quote output) ^ " " ^ (Filename.quote basef)
      in
      let st,_ = run_command av cmd in
      flash_info (cmd ^ pr_exit_status st)

exception DontQuit

let confirm_quit () =
  begin try save_pref() with e -> flash_info "Cannot save preferences" end;
  let ask_confirmation () =
    let res = GToolbox.question_box
      ~title:"Quit"
      ~buttons:["Save Named Buffers and Quit";
		"Quit without Saving";
		"Don't Quit"]
      ~default:0
      ~icon:warn_image
      "There are unsaved buffers"
    in
    match res with
      | 1 -> saveall ()
      | 2 -> ()
      | _ -> raise DontQuit
  in
  try
    if List.exists (fun p -> p.script#buffer#modified) session_notebook#pages
    then ask_confirmation ();
    List.iter (fun p -> Coq.close_coqtop p.toplvl) session_notebook#pages;
    true
  with DontQuit -> false

let quit _ = if confirm_quit () then exit 0

let close_buffer _ =
  let do_remove () =
    let c = session_notebook#current_page in
    session_notebook#remove_page c
  in
  let current = session_notebook#current_term in
  if not current.script#buffer#modified then do_remove ()
  else
    match GToolbox.question_box ~title:"Close"
      ~buttons:["Save Buffer and Close";
                "Close without Saving";
                "Don't Close"]
      ~default:0
      ~icon:warn_image
      "This buffer has unsaved modifications"
    with
      | 1 ->
        begin match current.analyzed_view#filename with
          | None ->
            begin match select_file_for_save ~title:"Save file" () with
              | None -> ()
              | Some f ->
                if current.analyzed_view#save_as f then begin
                  flash_info ("File " ^ f ^ " saved") ;
                  do_remove ()
                end else
                  warning  ("Save Failed (check if " ^ f ^ " is writable)")
            end
          | Some f ->
            if current.analyzed_view#save f then begin
              flash_info ("File " ^ f ^ " saved") ;
              do_remove ()
            end else
              warning  ("Save Failed (check if " ^ f ^ " is writable)")
        end
      | 2 -> do_remove ()
      | _ -> ()

let print _ =
  let av = current_view () in
  match av#filename with
    |None -> flash_info "Cannot print: this buffer has no name"
    |Some f_name ->
      let cmd =
        local_cd f_name ^ current.cmd_coqdoc ^ " -ps " ^
	Filename.quote (Filename.basename f_name) ^ " | " ^ current.cmd_print
      in
      let print_window = GWindow.window ~title:"Print" ~modal:true
	~position:`CENTER ~wm_class:"CoqIDE" ~wm_name:"CoqIDE" ()
      in
      let vbox_print = GPack.vbox ~spacing:10 ~border_width:10
	~packing:print_window#add ()
      in
      let _ = GMisc.label ~text:"Print using the following command:"
	 ~justify:`LEFT ~packing:vbox_print#add	()
      in
      let print_entry = GEdit.entry ~text:cmd ~editable:true
	~width_chars:80 ~packing:vbox_print#add ()
      in
      let hbox_print = GPack.hbox ~spacing:10 ~packing:vbox_print#add ()
      in
      let cancel_button = GButton.button ~stock:`CANCEL ~label:"Cancel"
	~packing:hbox_print#add ()
      in
      let print_button  = GButton.button ~stock:`PRINT ~label:"Print"
	~packing:hbox_print#add ()
      in
      let callback_print () =
        let cmd = print_entry#text in
        let st,_ = run_command av cmd in
        flash_info (cmd ^ pr_exit_status st);
        print_window#destroy ()
      in
      let _ = cancel_button#connect#clicked ~callback:print_window#destroy in
      let _ = print_button#connect#clicked ~callback:callback_print in
      print_window#misc#show ()

let highlight _ =
  let trm = session_notebook#current_term in
  force_retag trm.script#buffer;
  trm.analyzed_view#recenter_insert

end

let confirm_quit = File.confirm_quit

(** Callbacks for external commands *)

module External = struct

let compile _ =
  let v = session_notebook#current_term in
  let av = v.analyzed_view in
  File.save ();
  match av#filename with
    |None -> flash_info "Active buffer has no name"
    |Some f ->
      let cmd = current.cmd_coqc ^ " -I "
	^ (Filename.quote (Filename.dirname f))
	^ " " ^ (Filename.quote f) in
      let st,res = run_command av cmd in
      if st = Unix.WEXITED 0 then
	flash_info (f ^ " successfully compiled")
      else begin
	flash_info (f ^ " failed to compile");
	Coq.try_grab v.toplvl av#process_until_end_or_error ignore;
	av#insert_message "Compilation output:\n";
	av#insert_message res
      end

let make _ =
  let av = current_view () in
  match av#filename with
    |None -> flash_info "Cannot make: this buffer has no name"
    |Some f ->
      let cmd = local_cd f ^ current.cmd_make in
      (*
	File.save ();
      *)
      av#insert_message "Command output:\n";
      let st,res = run_command av cmd in
      last_make := res;
      last_make_index := 0;
      flash_info (current.cmd_make ^ pr_exit_status st)

let next_error _ =
  try
    let file,line,start,stop,error_msg = search_next_error () in
    do_load file;
    let v = session_notebook#current_term in
    let av = v.analyzed_view in
    let input_buffer = v.script#buffer in
      (*
	let init = input_buffer#start_iter in
	let i = init#forward_lines (line-1) in
      *)
      (*
	let convert_pos = byte_offset_to_char_offset phrase in
	let start = convert_pos start in
	let stop = convert_pos stop in
      *)
      (*
	let starti = i#forward_chars start in
	let stopi = i#forward_chars stop in
      *)
    let starti = input_buffer#get_iter_at_byte ~line:(line-1) start in
    let stopi = input_buffer#get_iter_at_byte ~line:(line-1) stop in
    input_buffer#apply_tag Tags.Script.error
      ~start:starti
      ~stop:stopi;
    input_buffer#place_cursor ~where:starti;
    av#set_message error_msg;
    v.script#misc#grab_focus ()
  with Not_found ->
    last_make_index := 0;
    (current_view ())#set_message "No more errors.\n"

let coq_makefile _ =
  let av = current_view () in
  match av#filename with
    |None -> flash_info "Cannot make makefile: this buffer has no name"
    |Some f ->
      let cmd = local_cd f ^ current.cmd_coqmakefile in
      let st,res = run_command av cmd in
      flash_info (current.cmd_coqmakefile ^ pr_exit_status st)

let editor _ =
  let av = current_view () in
  match av#filename with
    |None -> warning "Call to external editor available only on named files"
    |Some f ->
      File.save ();
      let com =
	Util.subst_command_placeholder current.cmd_editor (Filename.quote f)
      in
      let _ = run_command av com in
      av#revert
end

let detach_view _ =
  (* Open a separate window containing the current buffer *)
  let trm = session_notebook#current_term in
  let file = match trm.analyzed_view#filename with
    |None -> "*scratch*"
    |Some f -> f
  in
  let w = GWindow.window ~show:true
    ~width:(current.window_width*2/3)
    ~height:(current.window_height*2/3)
    ~position:`CENTER
    ~title:file
    ()
  in
  let sb = GBin.scrolled_window ~packing:w#add () in
  let nv = GText.view ~buffer:trm.script#buffer ~packing:sb#add () in
  nv#misc#modify_font current.text_font;
  (* If the buffer in the main window is closed, destroy this detached view *)
  ignore (trm.script#connect#destroy ~callback:w#destroy)

let initial_about () =
  let initial_string =
    "Welcome to CoqIDE, an Integrated Development Environment for Coq"
  in
  let coq_version = Coq.short_version () in
  let version_info =
    if Glib.Utf8.validate coq_version then
      "\nYou are running " ^ coq_version
    else ""
  in
  let msg = initial_string ^ version_info in
  session_notebook#current_term.message_view#push Interface.Notice msg

let coq_icon () =
  (* May raise Nof_found *)
  let name = "coq.png" in
  let chk d = Sys.file_exists (Filename.concat d name) in
  let dir = List.find chk (Minilib.coqide_data_dirs ()) in
  Filename.concat dir name

let about _ =
  let dialog = GWindow.about_dialog () in
  let _ = dialog#connect#response (fun _ -> dialog#destroy ()) in
  let _ =
    try dialog#set_logo (GdkPixbuf.from_file (coq_icon ()))
    with _ -> ()
  in
  let copyright =
    "Coq is developed by the Coq Development Team\n" ^
    "(INRIA - CNRS - LIX - LRI - PPS)"
  in
  let authors = [
    "Benjamin Monate";
    "Jean-Christophe Filliâtre";
    "Pierre Letouzey";
    "Claude Marché";
    "Bruno Barras";
    "Pierre Corbineau";
    "Julien Narboux";
    "Hugo Herbelin";
  ]
  in
  dialog#set_name "CoqIDE";
  dialog#set_comments "The Coq Integrated Development Environment";
  dialog#set_website Coq_config.wwwcoq;
  dialog#set_version Coq_config.version;
  dialog#set_copyright copyright;
  dialog#set_authors authors;
  dialog#show ()

(** Callbacks for the Navigation menu *)

let update_status h k =
  let display msg = pop_info (); push_info msg
  in
  Coq.status h (function
    |Interface.Fail (l, str) ->
      display "Oops, problem while fetching coq status."; k ()
    |Interface.Good status | Interface.Unsafe status ->
      let path = match status.Interface.status_path with
        | [] | _ :: [] -> "" (* Drop the topmost level, usually "Top" *)
        | _ :: l -> " in " ^ String.concat "." l
      in
      let name = match status.Interface.status_proofname with
        | None -> ""
        | Some n -> ", proving " ^ n
      in
      display ("Ready" ^ path ^ name); k ())

let send_to_coq f =
  let term = session_notebook#current_term in
  let av = term.analyzed_view in
  let info () = Minilib.log ("Coq busy, discarding query") in
  let f h k = f av h (fun () -> update_status h k) in
  Coq.try_grab term.toplvl f info

module Nav = struct
  let forward_one _ = send_to_coq (fun a -> a#process_next_phrase)
  let backward_one _ = send_to_coq (fun a -> a#backtrack_last_phrase)
  let goto _ = send_to_coq (fun a -> a#go_to_insert)
  let restart _ = force_reset_initial ()
  let goto_end _ = send_to_coq (fun a -> a#process_until_end_or_error)
  let interrupt _ = break ()
  let previous_occ _ = (current_view ())#find_next_occurrence Up
  let next_occ _ = (current_view ())#find_next_occurrence Down
end

let tactic_wizard_callback l _ = send_to_coq (fun a -> a#tactic_wizard l)

let printopts_callback opts v =
  let opts = List.map (fun o -> (o,v#get_active)) opts in
  send_to_coq (fun av h k ->
    Coq.PrintOpt.set opts h
      (fun () -> av#show_goals h k))

(** Templates menu *)

let match_template _ =
  let w = get_current_word () in
  let coqtop = session_notebook#current_term.toplvl in
  let display_match k = function
    |Interface.Fail _ -> (flash_info "Not an inductive type"; k ())
    |Interface.Good cases | Interface.Unsafe cases ->
      k ();
      let print_branch c l =
	Format.fprintf c " | @[<hov 1>%a@]=> _@\n"
	  (print_list (fun c s -> Format.fprintf c "%s@ " s)) l
      in
      let b = Buffer.create 1024 in
      let fmt = Format.formatter_of_buffer b in
      Format.fprintf fmt "@[match var with@\n%aend@]@."
        (print_list print_branch) cases;
      let s = Buffer.contents b in
      Minilib.log s;
      let {script = view } = session_notebook#current_term in
      ignore (view#buffer#delete_selection ());
      let m = view#buffer#create_mark
        (view#buffer#get_iter `INSERT)
      in
      if view#buffer#insert_interactive s then
        let i = view#buffer#get_iter (`MARK m) in
        let _ = i#nocopy#forward_chars 9 in
        view#buffer#place_cursor ~where:i;
        view#buffer#move_mark ~where:(i#backward_chars 3) `SEL_BOUND
  in
  Coq.try_grab coqtop (fun h k -> Coq.mkcases w h (display_match k)) ignore

(** Queries *)

module Query = struct

let searchabout () =
  let word = get_current_word () in
  let term = session_notebook#current_term in
  let buf =  term.message_view#buffer in
  let insert result =
    let qualid = result.Interface.coq_object_qualid in
    let name = String.concat "." qualid in
    let tpe = result.Interface.coq_object_object in
    buf#insert ~tags:[Tags.Message.item] name;
    buf#insert "\n";
    buf#insert tpe;
    buf#insert "\n";
  in
  let display_results k r =
    term.message_view#clear ();
    List.iter insert (match r with Interface.Good l -> l | _ -> []);
    k ()
  in
  let launch_query h k =
    Coq.search [Interface.SubType_Pattern word, true] h (display_results k)
  in
  Coq.try_grab term.toplvl launch_query ignore

let otherquery command _ =
  let word = get_current_word () in
  let term = session_notebook#current_term in
  let f query = term.analyzed_view#raw_coq_query query in
  if not (word = "") then
    let query = command ^ " " ^ word ^ "." in
    term.message_view#clear ();
    try Coq.try_grab term.toplvl (f query) ignore
    with e -> term.message_view#push Interface.Error (Printexc.to_string e)

let query command _ =
  if command = "SearchAbout"
  then searchabout ()
  else otherquery command ()

end

(** The [main] function *)

let main files =

  (* Main window *)
  let w = GWindow.window
    ~wm_class:"CoqIde" ~wm_name:"CoqIde"
    ~allow_grow:true ~allow_shrink:true
    ~width:current.window_width ~height:current.window_height
    ~title:"CoqIde" ()
  in
  (try w#set_icon (Some (GdkPixbuf.from_file (coq_icon ())))
   with _ -> ());

  let vbox = GPack.vbox ~homogeneous:false ~packing:w#add () in

  let emit_to_focus sgn =
    let focussed_widget = GtkWindow.Window.get_focus w#as_window in
    let obj = Gobject.unsafe_cast focussed_widget in
    try GtkSignal.emit_unit obj sgn
    with _ -> ()
  in

(* begin Preferences *)
  let reset_revert_timer () =
    disconnect_revert_timer ();
    if current.global_auto_revert then
      revert_timer := Some
	(GMain.Timeout.add ~ms:current.global_auto_revert_delay
	   ~callback:(fun () -> File.revert_all (); true))
  in reset_revert_timer (); (* to enable statup preferences timer *)

  let reset_auto_save_timer () =
    let autosave {analyzed_view = av} = try av#auto_save with _ -> () in
    let autosave_all _ = List.iter autosave session_notebook#pages; true in
    disconnect_auto_save_timer ();
    if current.auto_save then
      auto_save_timer := Some
	(GMain.Timeout.add ~ms:current.auto_save_delay ~callback:autosave_all)
  in reset_auto_save_timer (); (* to enable statup preferences timer *)
(* end Preferences *)

  let file_actions = GAction.action_group ~name:"File" () in
  let edit_actions = GAction.action_group ~name:"Edit" () in
  let view_actions = GAction.action_group ~name:"View" () in
  let export_actions = GAction.action_group ~name:"Export" () in
  let navigation_actions = GAction.action_group ~name:"Navigation" () in
  let tactics_actions = GAction.action_group ~name:"Tactics" () in
  let templates_actions = GAction.action_group ~name:"Templates" () in
  let tools_actions = GAction.action_group ~name:"Tools" () in
  let queries_actions = GAction.action_group ~name:"Queries" () in
  let compile_actions = GAction.action_group ~name:"Compile" () in
  let windows_actions = GAction.action_group ~name:"Windows" () in
  let help_actions = GAction.action_group ~name:"Help" () in
  let add_gen_actions menu_name act_grp l =
    let no_under = Util.String.map (fun x -> if x = '_' then '-' else x) in
    let add_simple_template menu_name act_grp text =
      let text' =
	let l = String.length text - 1 in
	if String.get text l = '.'
	then text ^"\n"
	else text ^" "
      in
      GAction.add_action (menu_name^" "^(no_under text)) ~label:text
	~callback:
	(fun _ ->
	  let v = session_notebook#current_term.script in
	  ignore (v#buffer#insert_interactive text'))
	act_grp
    in
    List.iter (function
      | [] -> ()
      | [s] -> add_simple_template menu_name act_grp s
      | s::_ as ll -> let label = "_@..." in label.[1] <- s.[0];
		      GAction.add_action (menu_name^" "^(String.make 1 s.[0]))
			~label act_grp;
		      List.iter (add_simple_template menu_name act_grp) ll
    ) l
  in
  let tactic_shortcut s sc = GAction.add_action s ~label:("_"^s)
    ~accel:(current.modifier_for_tactics^sc)
    ~callback:(tactic_wizard_callback [s])
  in
  let query_shortcut s accel =
    GAction.add_action s ~label:("_"^s) ?accel ~callback:(Query.query s)
  in
  let add_complex_template (name, label, text, offset, len, key) =
    (* Templates/Lemma *)
    let callback _ =
      let v = session_notebook#current_term.script in
      if v#buffer#insert_interactive text then begin
	let iter = v#buffer#get_iter_at_mark `INSERT in
	ignore (iter#nocopy#backward_chars offset);
	v#buffer#move_mark `INSERT ~where:iter;
	ignore (iter#nocopy#backward_chars len);
	v#buffer#move_mark `SEL_BOUND ~where:iter;
      end in
      match key with
	|Some ac ->
	  let accel = current.modifier_for_templates^ac in
	  GAction.add_action name ~label ~callback ~accel
	|None -> GAction.add_action name ~label ~callback ?accel:None
  in
  GAction.add_actions file_actions [
    GAction.add_action "File" ~label:"_File";
    GAction.add_action "New" ~callback:File.newfile ~stock:`NEW;
    GAction.add_action "Open" ~callback:File.load ~stock:`OPEN;
    GAction.add_action "Save" ~callback:File.save ~stock:`SAVE
      ~tooltip:"Save current buffer";
    GAction.add_action "Save as" ~label:"S_ave as" ~stock:`SAVE_AS
      ~callback:File.saveas;
    GAction.add_action "Save all" ~label:"Sa_ve all"
      ~callback:File.saveall;
    GAction.add_action "Revert all buffers" ~label:"_Revert all buffers"
      ~stock:`REVERT_TO_SAVED ~callback:File.revert_all;
    GAction.add_action "Close buffer" ~label:"_Close buffer"
      ~callback:File.close_buffer
      ~stock:`CLOSE ~tooltip:"Close current buffer";
    GAction.add_action "Print..." ~label:"_Print..."
      ~callback:File.print ~stock:`PRINT ~accel:"<Ctrl>p";
    GAction.add_action "Rehighlight" ~label:"Reh_ighlight" ~accel:"<Ctrl>l"
      ~callback:File.highlight ~stock:`REFRESH;
    GAction.add_action "Quit" ~callback:File.quit ~stock:`QUIT;
  ];
  GAction.add_actions export_actions [
    GAction.add_action "Export to" ~label:"E_xport to";
    GAction.add_action "Html" ~label:"_Html" ~callback:(File.export "html");
    GAction.add_action "Latex" ~label:"_LaTeX" ~callback:(File.export "latex");
    GAction.add_action "Dvi" ~label:"_Dvi" ~callback:(File.export "dvi");
    GAction.add_action "Pdf" ~label:"_Pdf" ~callback:(File.export "pdf");
    GAction.add_action "Ps" ~label:"_Ps" ~callback:(File.export "ps");
  ];
  GAction.add_actions edit_actions [
    GAction.add_action "Edit" ~label:"_Edit";
    GAction.add_action "Undo" ~accel:"<Ctrl>u" ~stock:`UNDO
      ~callback:(fun _ -> session_notebook#current_term.script#undo ());
    GAction.add_action "Redo" ~stock:`REDO
      ~callback:(fun _ -> session_notebook#current_term.script#redo ());
    GAction.add_action "Cut"
      ~callback:(fun _ -> emit_to_focus GtkText.View.S.cut_clipboard)
      ~stock:`CUT;
    GAction.add_action "Copy"
      ~callback:(fun _ -> emit_to_focus GtkText.View.S.copy_clipboard)
      ~stock:`COPY;
    GAction.add_action "Paste"
      ~callback:(fun _ -> emit_to_focus GtkText.View.S.paste_clipboard)
      ~stock:`PASTE;
    GAction.add_action "Find" ~stock:`FIND
      ~callback:(fun _ -> session_notebook#current_term.finder#show `FIND);
    GAction.add_action "Find Next" ~label:"Find _Next" ~stock:`GO_DOWN
      ~accel:"F3"
      ~callback:(fun _ -> session_notebook#current_term.finder#find_forward ());
    GAction.add_action "Find Previous" ~label:"Find _Previous" ~stock:`GO_UP
      ~accel:"<Shift>F3"
      ~callback:
      (fun _ -> session_notebook#current_term.finder#find_backward ());
    GAction.add_action "Replace" ~stock:`FIND_AND_REPLACE
      ~callback:(fun _ -> session_notebook#current_term.finder#show `REPLACE);
    GAction.add_action "Close Find" ~accel:"Escape"
      ~callback:(fun _ -> session_notebook#current_term.finder#hide ());
    GAction.add_action "Complete Word" ~label:"Complete Word"
      ~callback:(fun _ ->
	ignore ( ()
(*	       let av = session_notebook#current_term.analyzed_view in
	       av#complete_at_offset (av#get_insert)#offset*)
	)) ~accel:"<Ctrl>slash";
    GAction.add_action "External editor" ~label:"External editor"
	~callback:External.editor ~stock:`EDIT;
    GAction.add_action "Preferences" ~accel:"<Ctrl>comma" ~stock:`PREFERENCES
      ~callback:(fun _ ->
	begin
	  try configure ~apply:update_notebook_pos ()
	  with _ -> flash_info "Cannot save preferences"
	end;
	reset_revert_timer ())];
  GAction.add_actions view_actions [
    GAction.add_action "View" ~label:"_View";
    GAction.add_action "Previous tab" ~label:"_Previous tab"
      ~accel:"<Alt>Left" ~stock:`GO_BACK
      ~callback:(fun _ -> session_notebook#previous_page ());
    GAction.add_action "Next tab" ~label:"_Next tab" ~accel:"<Alt>Right"
      ~stock:`GO_FORWARD
      ~callback:(fun _ -> session_notebook#next_page ());
    GAction.add_toggle_action "Show Toolbar" ~label:"Show _Toolbar"
      ~active:(current.show_toolbar)
      ~callback:
      (fun _ -> current.show_toolbar <- not current.show_toolbar;
        !refresh_toolbar_hook ());
    GAction.add_toggle_action "Show Query Pane" ~label:"Show _Query Pane"
      ~callback:(fun _ ->
	let ccw = session_notebook#current_term.command in
	if ccw#frame#misc#visible
	then ccw#frame#misc#hide ()
	else ccw#frame#misc#show ())
      ~accel:"<Alt>Escape";
  ];
  List.iter
    (fun (opts,name,label,key,dflt) ->
      GAction.add_toggle_action name ~active:dflt ~label
        ~accel:(current.modifier_for_display^key)
        ~callback:(printopts_callback opts)
	view_actions)
    print_items;
  GAction.add_actions navigation_actions [
    GAction.add_action "Navigation" ~label:"_Navigation";
    GAction.add_action "Forward" ~label:"_Forward" ~stock:`GO_DOWN
      ~callback:Nav.forward_one
      ~tooltip:"Forward one command"
      ~accel:(current.modifier_for_navigation^"Down");
    GAction.add_action "Backward" ~label:"_Backward" ~stock:`GO_UP
      ~callback:Nav.backward_one
      ~tooltip:"Backward one command"
      ~accel:(current.modifier_for_navigation^"Up");
    GAction.add_action "Go to" ~label:"_Go to" ~stock:`JUMP_TO
      ~callback:Nav.goto
      ~tooltip:"Go to cursor"
      ~accel:(current.modifier_for_navigation^"Right");
    GAction.add_action "Start" ~label:"_Start" ~stock:`GOTO_TOP
      ~callback:Nav.restart
      ~tooltip:"Restart coq" ~accel:(current.modifier_for_navigation^"Home");
    GAction.add_action "End" ~label:"_End" ~stock:`GOTO_BOTTOM
      ~callback:Nav.goto_end
      ~tooltip:"Go to end" ~accel:(current.modifier_for_navigation^"End");
    GAction.add_action "Interrupt" ~label:"_Interrupt" ~stock:`STOP
      ~callback:Nav.interrupt ~tooltip:"Interrupt computations"
      ~accel:(current.modifier_for_navigation^"Break");
(* wait for this available in GtkSourceView !
      GAction.add_action "Hide" ~label:"_Hide" ~stock:`MISSING_IMAGE
	~callback:(fun _ -> let sess = session_notebook#current_term in
		     toggle_proof_visibility sess.script#buffer
		       sess.analyzed_view#get_insert) ~tooltip:"Hide proof"
	~accel:(current.modifier_for_navigation^"h");*)
    GAction.add_action "Previous" ~label:"_Previous" ~stock:`GO_BACK
      ~callback:Nav.previous_occ
      ~tooltip:"Previous occurence"
      ~accel:(current.modifier_for_navigation^"less");
    GAction.add_action "Next" ~label:"_Next" ~stock:`GO_FORWARD
      ~callback:Nav.next_occ
      ~tooltip:"Next occurence"
      ~accel:(current.modifier_for_navigation^"greater");
  ];
  GAction.add_actions tactics_actions [
    GAction.add_action "Try Tactics" ~label:"_Try Tactics";
    GAction.add_action "Wizard" ~tooltip:"Proof Wizard"
      ~label:"<Proof Wizard>"
      ~stock:`DIALOG_INFO
      ~callback:(tactic_wizard_callback current.automatic_tactics)
      ~accel:(current.modifier_for_tactics^"dollar");
    tactic_shortcut "auto" "a";
    tactic_shortcut "auto with *" "asterisk";
    tactic_shortcut "eauto" "e";
    tactic_shortcut "eauto with *" "ampersand";
    tactic_shortcut "intuition" "i";
    tactic_shortcut "omega" "o";
    tactic_shortcut "simpl" "s";
    tactic_shortcut "tauto" "p";
    tactic_shortcut "trivial" "v";
  ];
  add_gen_actions "Tactic" tactics_actions Coq_commands.tactics;
  GAction.add_actions templates_actions [
    GAction.add_action "Templates" ~label:"Te_mplates";
    add_complex_template
      ("Lemma", "_Lemma __", "Lemma new_lemma : .\nProof.\n\nSave.\n",
       19, 9, Some "L");
    add_complex_template
      ("Theorem", "_Theorem __", "Theorem new_theorem : .\nProof.\n\nSave.\n",
       19, 11, Some "T");
    add_complex_template
      ("Definition", "_Definition __", "Definition ident := .\n",
       6, 5, Some "E");
    add_complex_template
      ("Inductive", "_Inductive __", "Inductive ident : :=\n  | : .\n",
       14, 5, Some "I");
    add_complex_template
      ("Fixpoint", "_Fixpoint __",
       "Fixpoint ident (_ : _) {struct _} : _ :=\n.\n",
       29, 5, Some "F");
    add_complex_template
      ("Scheme", "_Scheme __",
       "Scheme new_scheme := Induction for _ Sort _\n" ^
	 "with _ := Induction for _ Sort _.\n",
       61,10, Some "S");
    GAction.add_action "match" ~label:"match ..." ~callback:match_template
      ~accel:(current.modifier_for_templates^"C");
  ];
  add_gen_actions "Template" templates_actions Coq_commands.commands;
  GAction.add_actions queries_actions [
    GAction.add_action "Queries" ~label:"_Queries";
    query_shortcut "SearchAbout" (Some "F2");
    query_shortcut "Check" (Some "F3");
    query_shortcut "Print" (Some "F4");
    query_shortcut "About" (Some "F5");
    query_shortcut "Locate" None;
    query_shortcut "Print Assumptions" None;
    query_shortcut "Whelp Locate" None;
  ];
  GAction.add_actions tools_actions [
    GAction.add_action "Tools" ~label:"_Tools";
    GAction.add_action "Comment" ~label:"_Comment"
      ~callback:(fun _ -> session_notebook#current_term.script#comment ())
      ~accel:"<CTRL>D";
    GAction.add_action "Uncomment" ~label:"_Uncomment"
      ~callback:(fun _ -> session_notebook#current_term.script#uncomment ())
      ~accel:"<CTRL><SHIFT>D";
  ];
  GAction.add_actions compile_actions [
    GAction.add_action "Compile" ~label:"_Compile";
    GAction.add_action "Compile buffer" ~label:"_Compile buffer"
      ~callback:External.compile;
    GAction.add_action "Make" ~label:"_Make" ~accel:"F6"
      ~callback:External.make;
    GAction.add_action "Next error" ~label:"_Next error" ~accel:"F7"
      ~callback:External.next_error;
    GAction.add_action "Make makefile" ~label:"Make makefile"
      ~callback:External.coq_makefile;
  ];
  GAction.add_actions windows_actions [
    GAction.add_action "Windows" ~label:"_Windows";
    GAction.add_action "Detach View" ~label:"Detach _View"
      ~callback:detach_view
  ];
  GAction.add_actions help_actions [
    GAction.add_action "Help" ~label:"_Help";
    GAction.add_action "Browse Coq Manual" ~label:"Browse Coq _Manual"
      ~callback:(fun _ -> browse (current_view ())#insert_message (doc_url ()));
    GAction.add_action "Browse Coq Library" ~label:"Browse Coq _Library"
      ~callback:
      (fun _ -> browse (current_view ())#insert_message current.library_url);
    GAction.add_action "Help for keyword" ~label:"Help for _keyword"
      ~callback:(fun _ -> (current_view ())#help_for_keyword ()) ~stock:`HELP;
    GAction.add_action "About Coq" ~label:"_About" ~stock:`ABOUT
      ~callback:about;
  ];
  Coqide_ui.init ();
  Coqide_ui.ui_m#insert_action_group file_actions 0;
  Coqide_ui.ui_m#insert_action_group export_actions 0;
  Coqide_ui.ui_m#insert_action_group edit_actions 0;
  Coqide_ui.ui_m#insert_action_group view_actions 0;
  Coqide_ui.ui_m#insert_action_group navigation_actions 0;
  Coqide_ui.ui_m#insert_action_group tactics_actions 0;
  Coqide_ui.ui_m#insert_action_group templates_actions 0;
  Coqide_ui.ui_m#insert_action_group tools_actions 0;
  Coqide_ui.ui_m#insert_action_group queries_actions 0;
  Coqide_ui.ui_m#insert_action_group compile_actions 0;
  Coqide_ui.ui_m#insert_action_group windows_actions 0;
  Coqide_ui.ui_m#insert_action_group help_actions 0;
  w#add_accel_group Coqide_ui.ui_m#get_accel_group ;
  GtkMain.Rc.parse_string "gtk-can-change-accels = 1";
  if Coq_config.gtk_platform <> `QUARTZ
  then vbox#pack (Coqide_ui.ui_m#get_widget "/CoqIde MenuBar");
  let tbar = GtkButton.Toolbar.cast
    ((Coqide_ui.ui_m#get_widget "/CoqIde ToolBar")#as_widget)
  in
  let () = GtkButton.Toolbar.set ~orientation:`HORIZONTAL ~style:`ICONS
    ~tooltips:true tbar in
  let toolbar = new GObj.widget tbar in
  vbox#pack toolbar;

  ignore (w#event#connect#delete ~callback:(fun _ -> File.quit (); true));

  (* Reset on tab switch *)
  ignore (session_notebook#connect#switch_page
    (fun _ -> if current.reset_on_tab_switch then force_reset_initial ()));
  (* The vertical Separator between Scripts and Goals *)
  vbox#pack ~expand:true session_notebook#coerce;
  update_notebook_pos ();
  let lower_hbox = GPack.hbox ~homogeneous:false ~packing:vbox#pack () in
  lower_hbox#pack ~expand:true status#coerce;
  push_info "Ready";
  (* Location display *)
  let l = GMisc.label
    ~text:"Line:     1 Char:   1"
    ~packing:lower_hbox#pack () in
  l#coerce#misc#set_name "location";
  set_location := l#set_text;
  (* Progress Bar *)
  let pbar = GRange.progress_bar ~pulse_step:0.2 () in
  lower_hbox#pack pbar#coerce;
  pbar#set_text "CoqIde started";
  let _ = Glib.Timeout.add ~ms:300
    ~callback:(fun () ->
      if Coq.is_computing session_notebook#current_term.toplvl
      then pbar#pulse ();
      true)
  in

  (* Initializing hooks *)

  refresh_toolbar_hook :=
    (fun () ->
      if current.show_toolbar
      then toolbar#misc#show ()
      else toolbar#misc#hide ());
  refresh_style_hook :=
    (fun () ->
      let style =  style_manager#style_scheme current.source_style in
      let iter_page p = p.script#source_buffer#set_style_scheme style in
      List.iter iter_page session_notebook#pages;
    );
  refresh_editor_hook :=
    (fun () ->
      let wrap_mode = if current.dynamic_word_wrap then `WORD else `NONE in
      let show_spaces =
        if current.show_spaces then 0b1001011 (* SPACE, TAB, NBSP, TRAILING *)
        else 0
      in
      let fd = current.text_font in
      let clr = Tags.color_of_string current.background_color in

      let iter_page p =
        (* Editor settings *)
        p.script#set_wrap_mode wrap_mode;
        p.script#set_show_line_numbers current.show_line_number;
        p.script#set_auto_indent current.auto_indent;
        p.script#set_highlight_current_line current.highlight_current_line;

        (* Hack to handle missing binding in lablgtk *)
        let conv =
	  { Gobject.name = "draw-spaces"; Gobject.conv = Gobject.Data.int }
	in
        Gobject.set conv p.script#as_widget show_spaces;

        p.script#set_show_right_margin current.show_right_margin;
        p.script#set_insert_spaces_instead_of_tabs
	  current.spaces_instead_of_tabs;
        p.script#set_tab_width current.tab_length;
        p.script#set_auto_complete current.auto_complete;

        (* Fonts *)
        p.script#misc#modify_font fd;
        p.proof_view#misc#modify_font fd;
        p.message_view#misc#modify_font fd;
        p.command#refresh_font ();

        (* Colors *)
        p.script#misc#modify_base [`NORMAL, `COLOR clr];
        p.proof_view#misc#modify_base [`NORMAL, `COLOR clr];
        p.message_view#misc#modify_base [`NORMAL, `COLOR clr];
        p.command#refresh_color ()

      in
      List.iter iter_page session_notebook#pages;
    );
  resize_window_hook := (fun () ->
    w#resize
      ~width:current.window_width
      ~height:current.window_height);
  refresh_tabs_hook := update_notebook_pos;
  (* Remove default pango menu for textviews *)
  w#show ();

(* Begin Color configuration *)

  Tags.set_processing_color (Tags.color_of_string current.processing_color);
  Tags.set_processed_color (Tags.color_of_string current.processed_color);

(* End of color configuration *)

  if List.length files >=1 then
    begin
      List.iter (fun f ->
	if Sys.file_exists f then do_load f else
          let f = if Filename.check_suffix f ".v" then f else f^".v" in
	  load_file (fun s -> print_endline s; exit 1) f)
        files;
      session_notebook#goto_page 0;
    end
  else
    begin
      let session = create_session None in
      let index = session_notebook#append_term session in
      !refresh_editor_hook ();
      session_notebook#goto_page index;
    end;
  initial_about ();
  !refresh_toolbar_hook ();
  !refresh_editor_hook ();
  session_notebook#current_term.script#misc#grab_focus ();
  Minilib.log "End of Coqide.main"

(* This function check every half of second if GeoProof has send
   something on his private clipboard *)

let check_for_geoproof_input () =
  let cb_Dr = GData.clipboard (Gdk.Atom.intern "_GeoProof") in
  let handler () =
    match cb_Dr#text with
      | None -> true
      | Some "Ack" -> true
      | Some s ->
	session_notebook#current_term.script#buffer#insert (s^"\n");
	(* cb_Dr#clear does not work so i use : *)
	cb_Dr#set_text "Ack";
	true
  in
  ignore (GMain.Timeout.add ~ms:100 ~callback:handler)

(** By default, the coqtop we try to launch is exactly the current coqide
    full name, with the last occurrence of "coqide" replaced by "coqtop".
    This should correctly handle the ".opt", ".byte", ".exe" situations.
    If the replacement fails, we default to "coqtop", hoping it's somewhere
    in the path. Note that the -coqtop option to coqide allows to override
    this default coqtop path *)

let read_coqide_args argv =
  let rec filter_coqtop coqtop project_files out = function
    |"-coqtop" :: prog :: args ->
      if coqtop = None then filter_coqtop (Some prog) project_files out args
      else (output_string stderr "Error: multiple -coqtop options"; exit 1)
    |"-f" :: file :: args ->
      let d = CUnix.canonical_path_name (Filename.dirname file) in
      let p = Project_file.read_project_file file in
      filter_coqtop coqtop ((d,p) :: project_files) out args
    |"-f" :: [] ->
      output_string stderr "Error: missing project file name"; exit 1
    |"-coqtop" :: [] ->
      output_string stderr "Error: missing argument after -coqtop"; exit 1
    |"-debug"::args ->
      Minilib.debug := true;
      filter_coqtop coqtop project_files ("-debug"::out) args
    |arg::args -> filter_coqtop coqtop project_files (arg::out) args
    |[] -> (coqtop,List.rev project_files,List.rev out)
  in
  let coqtop,project_files,argv = filter_coqtop None [] [] argv in
  Ideutils.custom_coqtop := coqtop;
  custom_project_files := project_files;
  argv
