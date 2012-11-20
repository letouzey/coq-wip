(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)


open Preferences

exception Forbidden

(* status bar and locations *)

let status = GMisc.statusbar ()

let push_info,pop_info,clear_info =
  let status_context = status#new_context ~name:"Messages" in
  let size = ref 0 in
  (fun s -> incr size; ignore (status_context#push s)),
  (fun () -> decr size; status_context#pop ()),
  (fun () -> for i = 1 to !size do status_context#pop () done; size := 0)

let flash_info =
  let flash_context = status#new_context ~name:"Flash" in
    (fun ?(delay=5000) s -> flash_context#flash ~delay s)



let set_location = ref  (function s -> failwith "not ready")

let pbar = GRange.progress_bar ~pulse_step:0.2 ()

let debug = ref (false)

let prerr_endline s =
  if !debug then try prerr_endline s;flush stderr with _ -> ()

let get_insert input_buffer = input_buffer#get_iter_at_mark `INSERT

let is_char_start c = let code = Char.code c in code < 0x80 || code >= 0xc0

let byte_offset_to_char_offset s byte_offset =
  if (byte_offset < String.length s) then begin
    let count_delta = ref 0 in
    for i = 0 to byte_offset do
      let code = Char.code s.[i] in
      if code >= 0x80 && code < 0xc0 then incr count_delta
    done;
    byte_offset - !count_delta
  end
  else begin
    let count_delta = ref 0 in
    for i = 0 to String.length s - 1 do
      let code = Char.code s.[i] in
      if code >= 0x80 && code < 0xc0 then incr count_delta
    done;
    byte_offset - !count_delta
  end

let print_id id =
  prerr_endline ("GOT sig id :"^(string_of_int (Obj.magic id)))


let do_convert s =
  Utf8_convert.f
    (if Glib.Utf8.validate s then begin
      prerr_endline "Input is UTF-8";s
    end else
	let from_loc () =
	  let _,char_set = Glib.Convert.get_charset () in
	  flash_info
	    ("Converting from locale ("^char_set^")");
	  Glib.Convert.convert_with_fallback ~to_codeset:"UTF-8" ~from_codeset:char_set s
	in
	let from_manual enc =
	  flash_info
	    ("Converting from "^ enc);
	  Glib.Convert.convert s ~to_codeset:"UTF-8" ~from_codeset:enc
	in
	match !current.encoding with
	  |Preferences.Eutf8 | Preferences.Elocale -> from_loc ()
	  |Emanual enc ->
	    try
	      from_manual enc
	    with _ -> from_loc ())

let try_convert s =
  try
    do_convert s
  with _ ->
    "(* Fatal error: wrong encoding in input. \
Please choose a correct encoding in the preference panel.*)";;


let try_export file_name s =
  try let s =
    try match !current.encoding with
      |Eutf8 -> begin
	(prerr_endline "UTF-8 is enforced" ;s)
      end
      |Elocale -> begin
	let is_unicode,char_set = Glib.Convert.get_charset () in
	if is_unicode then
	  (prerr_endline "Locale is UTF-8" ;s)
	else
	  (prerr_endline ("Locale is "^char_set);
	   Glib.Convert.convert_with_fallback ~from_codeset:"UTF-8" ~to_codeset:char_set s)
      end
      |Emanual enc ->
	(prerr_endline ("Manual charset is "^ enc);
       Glib.Convert.convert_with_fallback ~from_codeset:"UTF-8" ~to_codeset:enc s)
    with e -> (prerr_endline ("Error ("^(Printexc.to_string e)^") in transcoding: falling back to UTF-8") ;s)
  in
  let oc = open_out file_name in
  output_string oc s;
  close_out oc;
  true
  with e -> prerr_endline (Printexc.to_string e);false

let my_stat f = try Some (Unix.stat f) with _ -> None

let revert_timer = ref None
let disconnect_revert_timer () = match !revert_timer with
  | None -> ()
  | Some id -> GMain.Timeout.remove id; revert_timer := None

let auto_save_timer = ref None
let disconnect_auto_save_timer () = match !auto_save_timer with
  | None -> ()
  | Some id -> GMain.Timeout.remove id; auto_save_timer := None

let highlight_timer = ref None
let set_highlight_timer f =
  match !highlight_timer with
    | None ->
	revert_timer :=
      Some (GMain.Timeout.add ~ms:2000
	      ~callback:(fun () -> f (); highlight_timer := None; true))
    | Some id ->
	GMain.Timeout.remove id;
	revert_timer :=
	Some (GMain.Timeout.add ~ms:2000
		~callback:(fun () -> f (); highlight_timer := None; true))

let last_dir = ref ""

let filter_all_files () = GFile.filter
  ~name:"All"
  ~patterns:["*"] ()

let filter_coq_files () =  GFile.filter
  ~name:"Coq source code"
  ~patterns:[ "*.v"] ()

let select_file_for_open ~title ?(dir = last_dir) ?(filename="") () =
  let file = ref None in
  let file_chooser = GWindow.file_chooser_dialog ~action:`OPEN ~modal:true ~title () in
    file_chooser#add_button_stock `CANCEL `CANCEL ;
    file_chooser#add_select_button_stock `OPEN `OPEN ;
    file_chooser#add_filter (filter_coq_files ());
    file_chooser#add_filter (filter_all_files ());
    file_chooser#set_default_response `OPEN;
    ignore (file_chooser#set_current_folder !dir);
    begin match file_chooser#run () with
      | `OPEN ->
	  begin
	    file := file_chooser#filename;
	    match !file with
		None -> ()
	      | Some s -> dir := Filename.dirname s;
	  end
      | `DELETE_EVENT | `CANCEL -> ()
    end ;
    file_chooser#destroy ();
    !file


let select_file_for_save ~title ?(dir = last_dir) ?(filename="") () =
  let file = ref None in
  let file_chooser = GWindow.file_chooser_dialog ~action:`SAVE ~modal:true ~title () in
    file_chooser#add_button_stock `CANCEL `CANCEL ;
    file_chooser#add_select_button_stock `SAVE `SAVE ;
    file_chooser#add_filter (filter_coq_files ());
    file_chooser#add_filter (filter_all_files ());
  (* this line will be used when a lablgtk >= 2.10.0 is the default on most distributions
       file_chooser#set_do_overwrite_confirmation true;
     *)
    file_chooser#set_default_response `SAVE;
    ignore (file_chooser#set_current_folder !dir);
    ignore (file_chooser#set_current_name filename);

    begin match file_chooser#run () with
      | `SAVE ->
	  begin
	    file := file_chooser#filename;
	    match !file with
		None -> ()
	      | Some s -> dir := Filename.dirname s;
	  end
      | `DELETE_EVENT | `CANCEL -> ()
    end ;
    file_chooser#destroy ();
    !file

let find_tag_start (tag :GText.tag) (it:GText.iter) =
  let it = it#copy in
  let tag = Some tag in
  while not (it#begins_tag tag) && it#nocopy#backward_char do
    ()
  done;
  it
let find_tag_stop (tag :GText.tag) (it:GText.iter) =
  let it = it#copy in
  let tag = Some tag in
  while not (it#ends_tag tag) && it#nocopy#forward_char do
    ()
  done;
  it
let find_tag_limits (tag :GText.tag) (it:GText.iter) =
 (find_tag_start tag it , find_tag_stop tag it)

let stock_to_widget ?(size=`DIALOG) s =
  let img = GMisc.image ()
  in img#set_stock s;
  img#coerce

let custom_coqtop = ref None

let coqtop_path () =
  let file = match !custom_coqtop with
    | Some s -> s
    | None ->
      match !current.cmd_coqtop with
	| Some s -> s
	| None ->
	  let prog = String.copy Sys.executable_name in
	  try
	    let pos = String.length prog - 6 in
	    let i = Str.search_backward (Str.regexp_string "coqide") prog pos in
	    String.blit "coqtop" 0 prog i 6;
	    prog
	  with Not_found -> "coqtop"
  in file

let rec print_list print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; print_list print fmt r

(* In win32, when a command-line is to be executed via cmd.exe
   (i.e. Sys.command, Unix.open_process, ...), it cannot contain several
   quoted "..." zones otherwise some quotes are lost. Solution: we re-quote
   everything. Reference: http://ss64.com/nt/cmd.html *)

let requote cmd = if Sys.os_type = "Win32" then "\""^cmd^"\"" else cmd

let absolute_filename f = Minilib.correct_path f (Sys.getcwd ())

let maxread = 1024

(* In a mono-thread coqide, we can use the same buffer for many [io_read_all] *)

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

let run_command display finally cmd =
  let cin = Unix.open_process_in cmd in
  let io_chan = Glib.Io.channel_of_descr (Unix.descr_of_in_channel cin) in
  let all_conds = [`ERR; `HUP; `IN; `NVAL; `PRI] in (* all except `OUT *)
  let rec has_errors = function
    | [] -> false
    | (`IN | `PRI) :: conds -> has_errors conds
    | e :: _ -> true
  in
  let handle_end () = finally (Unix.close_process_in cin); false
  in
  let handle_input conds =
    if has_errors conds then handle_end ()
    else
      let s = io_read_all io_chan in
      if s = "" then handle_end ()
      else (display (try_convert s); true)
  in
  ignore (Glib.Io.add_watch ~cond:all_conds ~callback:handle_input io_chan)


let browse prerr url =
  let com = Minilib.subst_command_placeholder !current.cmd_browse url in
  let finally = function
    | Unix.WEXITED 127 ->
      prerr
	("Could not execute:\n"^com^"\n"^
	 "check your preferences for setting a valid browser command\n")
    | _ -> ()
  in
  run_command (fun _ -> ()) finally com

let doc_url () =
  if !current.doc_url = use_default_doc_url || !current.doc_url = "" then
    let addr = List.fold_left Filename.concat (Coq_config.docdir) ["html";"refman";"index.html"] in
    if Sys.file_exists addr then "file://"^addr else Coq_config.wwwrefman
  else !current.doc_url

let url_for_keyword =
  let ht = Hashtbl.create 97 in
    lazy (
      begin try
	let cin =
	  try let index_urls = Filename.concat (List.find
            (fun x -> Sys.file_exists (Filename.concat x "index_urls.txt"))
	    Minilib.xdg_config_dirs) "index_urls.txt" in
	    open_in index_urls
	  with Not_found ->
	    let doc_url = doc_url () in
	    let n = String.length doc_url in
	      if n > 8 && String.sub doc_url 0 7 = "file://" then
		open_in (String.sub doc_url 7 (n-7) ^ "index_urls.txt")
	      else
		raise Exit
	in
	  try while true do
	    let s = input_line cin in
	      try
		let i = String.index s ',' in
		let k = String.sub s 0 i in
		let u = String.sub s (i + 1) (String.length s - i - 1) in
		  Hashtbl.add ht k u
	      with _ ->
		Minilib.safe_prerr_endline "Warning: Cannot parse documentation index file."
	  done with End_of_file ->
	    close_in cin
      with _ ->
	Minilib.safe_prerr_endline "Warning: Cannot find documentation index file."
      end;
      Hashtbl.find ht : string -> string)

let browse_keyword prerr text =
  try
    let u = Lazy.force url_for_keyword text in
    browse prerr (doc_url() ^ u)
  with Not_found -> prerr ("No documentation found for \""^text^"\".\n")
