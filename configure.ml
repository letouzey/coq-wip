(**********************************)

(**  Configuration script for Coq *)

(**********************************)

(** This file should be run via: ocaml configure.ml <opts> *)

#load "unix.cma"
open Printf

let coq_version = "trunk"
let vo_magic = 8511
let state_magic = 58511

(** * Utility functions *)

let die msg = eprintf "%s\nConfiguration script failed!\n" msg; exit 1

(** Shortcut *)

let int = int_of_string

(* TODO: check that input_line on win32 removes the \r
(** Remove the final '\r' that may exists on Win32 *)

let remove_final_cr s =
  let n = String.length s in
  if n<>0 && s.[n-1] = '\r' then String.sub s 0 (n-1)
  else s
*)

(** Run some unix command and read the first line of its stdout *)

(* TODO: capture and discard stderr (at least in case of non-fatal) *)

let run ?(fatal=true) cmd =
  try
    let chan = Unix.open_process_in cmd in
    let line = (*remove_final_cr*) (input_line chan) in
    match Unix.close_process_in chan with
    | Unix.WEXITED 0 -> line
    | _ -> failwith "process failed, killed or stopped"
  with
  | End_of_file -> ""
  | _ when fatal -> die ("Error while running: "^cmd)
  | _ -> ""

let tryrun cmd = run ~fatal:false cmd

(** Splitting a string at some character *)

let string_split c s =
  let len = String.length s in
  let rec split n =
    try
      let pos = String.index_from s n c in
      let dir = String.sub s n (pos-n) in
      dir :: split (succ pos)
    with
      | Not_found -> [String.sub s n (len-n)]
  in
  if len = 0 then [] else split 0

(** String prefix test : does [s1] starts with [s2] ? *)

let starts_with s1 s2 =
  let l1 = String.length s1 and l2 = String.length s2 in
  l2 <= l1 && s2 = String.sub s1 0 l2

(** Turn a version string such as "4.01.0+rc2" into the list
    ["4";"01";"1"], stopping at the first non-digit or "." *)

let numeric_prefix_list s =
  let isnum c = (c = '.' || (c >= '0' && c <= '9')) in
  let max = String.length s in
  let i = ref 0 in
  while !i < max && isnum s.[!i] do incr i done;
  string_split '.' (String.sub s 0 !i)

(** Combined existence and directory tests *)

let dir_exists f = Sys.file_exists f && Sys.is_directory f

(** Does a file exist and is executable ? *)

let is_executable f =
  try let () = Unix.access f [Unix.X_OK] in true
  with Unix.Unix_error _ -> false

(** The PATH list for searching programs *)

let global_path =
  try string_split ':' (Sys.getenv "PATH")
  with Not_found -> []

(** A "which" command. May raise [Not_found] *)

(* TODO: .exe on win32 ? TODO: ";" as delimitor on win32 ? *)

let which ?(path=global_path) prog =
  let rec search = function
    | [] -> raise Not_found
    | dir :: path ->
      let file = if dir = "" then "./"^prog else dir^"/"^prog in
      if is_executable file then file else search path
  in search path

let program_in_path prog =
  try let _ = which prog in true with Not_found -> false


(** * Date *)

(** The short one is displayed when starting coqtop,
    The long one is used as compile date *)

let months =
 [| "January";"February";"March";"April";"May";"June";
    "July";"August";"September";"October";"November";"December" |]

let get_date () =
  let now = Unix.localtime (Unix.time ()) in
  let year = 1900+now.Unix.tm_year in
  let month = months.(now.Unix.tm_mon) in
  sprintf "%s %d" month year,
  sprintf "%s %d %d %d:%d:%d" (String.sub month 0 3) now.Unix.tm_mday year
    now.Unix.tm_hour now.Unix.tm_min now.Unix.tm_sec

let short_date, full_date = get_date ()


(** Create the bin/ directory if non-existent *)

let _ = if not (dir_exists "bin") then Unix.mkdir "bin" 0o755


(** * Command-line parsing *)

type ide = Opt | Byte | No

let get_bool = function
  | "true" | "yes" | "y" | "all" -> true
  | "false" | "no" | "n" -> false
  | s -> raise (Arg.Bad ("boolean argument expected instead of "^s))

let get_ide = function
  | "opt" -> Opt
  | "byte" -> Byte
  | "no" -> No
  | s -> raise (Arg.Bad ("(opt|byte|no) argument expected instead of "^s))

let arg_bool r = Arg.String (fun s -> r := get_bool s)

let arg_string_option r = Arg.String (fun s -> r := Some s)

module Prefs = struct
  let prefix = ref (None : string option)
  let local = ref false
  let coqrunbyteflags = ref (None : string option)
  let coqtoolsbyteflags = ref (None : string option)
  let custom = ref false
  let bindir = ref (None : string option)
  let libdir = ref (None : string option)
  let configdir = ref (None : string option)
  let datadir = ref (None : string option)
  let mandir = ref (None : string option)
  let docdir = ref (None : string option)
  let emacslib = ref (None : string option)
  let coqdocdir = ref (None : string option)
  let camldir = ref (None : string option)
  let lablgtkdir = ref (None : string option)
  let usecamlp5 = ref true
  let camlp5dir = ref (None : string option)
  let arch = ref (None : string option)
  let opt = ref false
  let natdynlink = ref true
  let coqide = ref (None : ide option)
  let macintegration = ref true
  let browser = ref (None : string option)
  let withdoc = ref true
  let geoproof = ref false
  let byteonly = ref false
  let debug = ref false
  let profile = ref false
  let annotate = ref false
  let typerex = ref false
  let makecmd = ref "make"
  let nativecompiler = ref true
  let coqwebsite = ref "http://coq.inria.fr/"
  let force_caml_version = ref false
end

(* TODO : earlier any option -foo was also available as --foo *)

let args_options = [
  "-prefix", arg_string_option Prefs.prefix,
    "<dir>\t\tSet installation directory to <dir>";
  "-local", Arg.Set Prefs.local,
    "\t\tSet installation directory to the current source tree";
  "-coqrunbyteflags", arg_string_option Prefs.coqrunbyteflags,
    "<flags>\tSet link flags for VM-dependent bytecode (coqtop)";
  "-coqtoolsbyteflags", arg_string_option Prefs.coqtoolsbyteflags,
    "<flags>\tSet link flags for VM-independant bytecode (coqdep,coqdoc,...)";
  "-custom", Arg.Set Prefs.custom,
    "\t\tGenerate all bytecode executables with -custom (not recommended)";
  "-bindir", arg_string_option Prefs.bindir,
    "<dir>\t\tWhere to install bin files";
  "-libdir", arg_string_option Prefs.libdir,
    "<dir>\t\tWhere to install lib files";
  "-configdir", arg_string_option Prefs.configdir,
    "<dir>\tWhere to install config files";
  "-datadir", arg_string_option Prefs.datadir,
    "<dir>\tWhere to install data files";
  "-mandir", arg_string_option Prefs.mandir,
    "<dir>\t\tWhere to install man files";
  "-docdir", arg_string_option Prefs.docdir,
    "<dir>\t\tWhere to install doc files";
  "-emacslib", arg_string_option Prefs.emacslib,
    "<dir>\tWhere to install emacs files";
  "-emacs", Arg.String (fun s ->
      printf "Warning: obsolete -emacs option\n";
      Prefs.emacslib := Some s),
    "<dir>\t\t(Obsolete) same as -emacslib";
  "-coqdocdir", arg_string_option Prefs.coqdocdir,
    "<dir>\tWhere to install Coqdoc style files";
  "-camldir", arg_string_option Prefs.camldir,
    "<dir>\tSpecifies the path to the OCaml library";
  "-lablgtkdir", arg_string_option Prefs.lablgtkdir,
    "<dir>\tSpecifies the path to the Lablgtk library";
  "-usecamlp5", Arg.Set Prefs.usecamlp5,
    "\t\tSpecifies to use camlp5 instead of camlp4";
  "-usecamlp4", Arg.Clear Prefs.usecamlp5,
    "\t\tSpecifies to use camlp4 instead of camlp5";
  "-camlp5dir",
    Arg.String (fun s -> Prefs.usecamlp5:=true; Prefs.camlp5dir:=Some s),
    "<dir>\tSpecifies where to look for the Camlp5 library and tells to use it";
  "-arch", arg_string_option Prefs.arch,
    "<arch>\t\tSpecifies the architecture";
  "-opt", Arg.Set Prefs.opt,
    "\t\t\tUse OCaml *.opt optimized compilers or not";
  "-natdynlink", arg_bool Prefs.natdynlink,
    "(yes|no)\tUse dynamic loading of native code or not";
  "-coqide", Arg.String (fun s -> Prefs.coqide := Some (get_ide s)),
    "(opt|byte|no)\tSpecifies whether or not to compile Coqide";
  "-nomacintegration", Arg.Clear Prefs.macintegration,
    "\tDo not try to build coqide mac integration";
  "-browser", arg_string_option Prefs.browser,
    "<command>\tUse <command> to open URL %s";
  "-nodoc", Arg.Clear Prefs.withdoc,
    "\t\tDo not compile the documentation";
  "-with-doc", arg_bool Prefs.withdoc,
    "(yes|no)\tCompile the documentation or not";
  "-with-geoproof", arg_bool Prefs.geoproof,
    "(yes|no)\tUse Geoproof binding or not";
  "-byte-only", Arg.Set Prefs.byteonly,
    "\t\tCompiles only bytecode version of Coq";
  "-byteonly", Arg.Set Prefs.byteonly,
    "\t\tCompiles only bytecode version of Coq";
  "-debug", Arg.Set Prefs.debug,
    "\t\tAdd debugging information in the Coq executables";
  "-profile", Arg.Set Prefs.profile,
    "\t\tAdd profiling information in the Coq executables";
  "-annotate", Arg.Set Prefs.annotate,
    "\t\tCompiles Coq with -dtypes option";
  "-typerex", Arg.Set Prefs.typerex,
    "\t\tCompiles Coq using typerex wrapper";
  "-makecmd", Arg.Set_string Prefs.makecmd,
    "<command>\tName of GNU Make command";
  "-no-native-compiler", Arg.Clear Prefs.nativecompiler,
    "\tDisables compilation to native code for conversion and normalization";
  "-coqwebsite", Arg.Set_string Prefs.coqwebsite,
    "\t\tURL of the coq website";
  "-force-caml-version", arg_bool Prefs.force_caml_version,
    "\tForce OCaml version";
]

let parse_args () =
  Arg.parse
    args_options
    (fun s -> raise (Arg.Bad ("Unknown option: "^s)))
    "Available options for configure are:";
  if !Prefs.local && !Prefs.prefix <> None then
    die "Options -prefix and -local are incompatible."

let _ = parse_args ()

(** Default OCaml binaries *)

type camlexec =
 { mutable byte : string;
   mutable opt : string;
   mutable top : string;
   mutable mklib : string;
   mutable dep : string;
   mutable doc : string;
   mutable lex : string;
   mutable yacc : string;
   mutable p4 : string }

(* TODO: autodetect .opt binaries ? *)

let camlexec =
  { byte = if !Prefs.opt then "ocamlc.opt" else "ocamlc";
    opt = if !Prefs.opt then "ocamlopt.opt" else "ocamlopt";
    top = "ocaml";
    mklib = "ocamlmklib";
    dep = "ocamldep";
    doc = "ocamldoc";
    lex = "ocamllex";
    yacc = "ocamlyacc";
    p4 = "camlp4o" }

let rebase_camlexec dir c =
  c.byte <- Filename.concat dir c.byte;
  c.opt <- Filename.concat dir c.opt;
  c.top <- Filename.concat dir c.top;
  c.mklib <- Filename.concat dir c.mklib;
  c.dep <- Filename.concat dir c.dep;
  c.doc <- Filename.concat dir c.doc;
  c.lex <- Filename.concat dir c.lex;
  c.yacc <- Filename.concat dir c.yacc;
  c.p4 <- Filename.concat dir c.p4

let coq_debug_flag = if !Prefs.debug then "-g" else ""
let coq_profile_flag = if !Prefs.profile then "-p" else ""
let coq_annotate_flag = if !Prefs.annotate then "-dtypes" else ""
let coq_typerex_wrapper =
  if !Prefs.typerex then "ocb-wrapper -save-types" else ""

let cflags = "-fno-defer-pop -Wall -Wno-unused"


(** * Architecture *)

let arch_progs =
  [("/bin/uname"," -s");
   ("/usr/bin/uname"," -s");
   ("/bin/arch", "");
   ("/usr/bin/arch", "");
   ("/usr/ucb/arch", "") ]

let query_arch () =
  printf "I can not automatically find the name of your architecture.\n";
  printf "Give me a name, please [win32 for Win95, Win98 or WinNT]: %!";
  read_line ()

let rec try_archs = function
  | (prog,opt)::rest when is_executable prog ->
    let arch = tryrun (prog^opt) in
    if arch <> "" then arch else try_archs rest
  | _ :: rest -> try_archs rest
  | [] -> query_arch ()

let cygwin = ref false

let arch = match !Prefs.arch with
  | Some a -> a
  | None ->
    let arch = tryrun "uname -s" in
    if starts_with arch "CYGWIN" then (cygwin := true; "win32")
    else if starts_with arch "MINGW32" then "win32"
    else if arch <> "" then arch
    else try_archs arch_progs

let exe,dll = if arch = "win32" then ".exe",".dll" else "", ".so"

(** * VCS

    Is the source tree checked out from a recognised
    Version Control System ? *)

let vcs =
  let git_dir = try Sys.getenv "GIT_DIR" with Not_found -> ".git" in
  if dir_exists git_dir then "git"
  else if Sys.file_exists ".svn/entries" then "svn"
  else if dir_exists "{arch}" then "gnuarch"
  else "none"

(** * The make command *)

let make =
  try
    let cmd = which ~path:(global_path@["."]) !Prefs.makecmd in
    let ver = List.nth (string_split ' ' (run (cmd^" -v"))) 2 in
    match string_split '.' ver with
    | major::minor::_ when (int major, int minor) >= (3,81) ->
      printf "You have GNU Make %s. Good!\n" ver
    | _ -> failwith "bad version"
  with _ -> die "Error: Cannot find GNU Make >= 3.81."

(** * Browser command *)

let browser =
  match !Prefs.browser with
  | Some b -> b
  | None when arch = "win32" -> "start %s"
  | None when arch = "Darwin" -> "open %s"
  | _ -> "firefox -remote \"OpenURL(%s,new-tab)\" || firefox %s &"

(** * OCaml programs *)

let camlbin, camlc = match !Prefs.camldir with
  | Some dir ->
    rebase_camlexec dir camlexec;
    Filename.dirname camlexec.byte, camlexec.byte
  | None ->
    try let camlc = which camlexec.byte in Filename.dirname camlc, camlc
    with Not_found ->
      printf "%s is not present in your path!\n" camlexec.byte;
      printf "Give me manually the path to the %s executable " camlexec.byte;
      printf "[/usr/local/bin by default]:\n%!";
      match read_line () with
      | "" -> let d = "/usr/local/bin" in d, Filename.concat d camlexec.byte
      | s when Filename.basename s = "ocamlc" -> Filename.dirname s, s
      | s when Filename.basename s = "ocamlc.opt" -> Filename.dirname s, s
      | s -> s, Filename.concat s camlexec.byte

let _ =
  if not (is_executable camlc) then
    die ("Error: cannot find the executable '"^camlc^"'.")

(* Under Windows, we need to convert from cygwin/mingw paths
   (e.g. /c/Program Files/Ocaml) to more windows-looking paths
   (c:/Program Files/Ocaml). Note that / are kept, since they are
   well supported now by Windows. *)

(* TODO: check the quoting under windows! *)

let mk_win_path file =
  if arch <> "win32" then file
  else if !cygwin then run ("cygpath -m "^Filename.quote file)
  else run (camlexec.top^" tools/mingwpath.ml "^Filename.quote file)

let camlbin = mk_win_path camlbin

let caml_version = run (Filename.quote camlc ^ " -version")
let camllib = run (Filename.quote camlc ^ " -where")
let camlp4compat = "-loc loc"

(** Caml version as a list of string, e.g. ["4";"00";"1"] *)

let caml_version_list = numeric_prefix_list caml_version

(** Same, with integers in the version list *)

let caml_version_nums =
  try
    if List.length caml_version_list < 2 then failwith "bad version";
    List.map int caml_version_list
  with _ ->
    die ("I found the OCaml compiler but cannot read its version number!\n" ^
         "Is it installed properly?")

let check_caml_version () =
  if caml_version_nums >= [3;11;2] then
    printf "You have OCaml %s. Good!\n" caml_version
  else
    let () = printf "Your version of OCaml is %s.\n" caml_version in
    if !Prefs.force_caml_version then
      printf "*Warning* Your version of OCaml is outdated.\n"
    else
      die "You need OCaml 3.11.2 or later."

let _ = check_caml_version ()

let coq_debug_flag_opt =
  if caml_version_nums >= [3;10] then coq_debug_flag else ""

let camltag = match caml_version_list with
  | x::y::_ -> "OCAML"^x^y
  | _ -> assert false


(** * CamlpX configuration *)

(** We assume that camlp(4|5) binaries are at the same place as ocaml ones
    (this should become configurable some day). *)

let camlp4bin = camlbin

(* TODO: camlp5dir should rather be the *binary* location, just as camldir *)
(* TODO: remove the late attempts at finding gramlib.cma *)

exception NoCamlp5

let check_camlp5 testcma = match !Prefs.camlp5dir with
  | Some dir ->
    if Sys.file_exists (dir^"/"^ testcma) then dir
    else
      let msg =
        sprintf "Cannot find camlp5 libraries in '%s' (%s not found)."
          dir testcma
      in die msg
  | None ->
    let dir = tryrun "camlp5 -where" in
    if dir <> "" then dir
    else if Sys.file_exists (camllib^"/camlp5/"^ testcma) then
      camllib^"/camlp5"
    else if Sys.file_exists (camllib^"/site-lib/camlp5/"^ testcma) then
      camllib^"/site-lib/camlp5"
    else
      let () = printf "No Camlp5 installation found." in
      let () = printf "Looking for Camlp4 instead...\n" in
      raise NoCamlp5

let check_camlp5_version () =
  let s = camlexec.p4 in
  (* translate 4 into 5 in the binary name *)
  for i = 0 to String.length s - 1 do
    if s.[i] = '4' then s.[i] <- '5'
  done;
  try
    (* TODO Quotes ? *)
    let ver = List.nth (string_split ' ' (run (camlexec.p4^" -v 2>&1"))) 2 in
    match string_split '.' ver with
    | major::minor::_ when (int major, int minor) >= (5,1) ->
      printf "You have Camlp5 %s. Good!\n" ver
    | _ -> failwith "bad version"
  with _ -> die "Error: unsupported Camlp5 (version < 5.01 or unrecognized).\n"

let config_camlpX () =
  try
    if not !Prefs.usecamlp5 then raise NoCamlp5;
    let lib = "gramlib" in
    let dir = check_camlp5 (lib^".cma") in
    let () = check_camlp5_version () in
    "camlp5", dir, lib
  with NoCamlp5 ->
    (* We now try to use Camlp4, either by explicit choice or
       by lack of proper Camlp5 installation *)
    let lib = "camlp4lib" in
    let dir = camllib^"/camlp4" in
    if not (Sys.file_exists (dir^"/"^lib^".cma")) then
      die "No Camlp4 installation found.\n";
    let () = camlexec.p4 <- camlexec.p4 ^ "rf" in
    ignore (run camlexec.p4);
    "camlp4", dir, lib

let camlp4, fullcamlp4lib, camlp4mod = config_camlpX ()

let shorten_camllib s =
  if starts_with s (camllib^"/") then
    let l = String.length camllib + 1 in
    "+" ^ String.sub s l (String.length s - l)
  else s

let camlp4lib = shorten_camllib fullcamlp4lib


(** * Native compiler *)

let msg_byteonly () =
  printf "Only the bytecode version of Coq will be available.\n"

let msg_no_ocamlopt () =
  printf "Cannot find the OCaml native-code compiler.\n"; msg_byteonly ()

let msg_no_camlp4_cmxa () =
  printf "Cannot find the native-code library of %s.\n" camlp4; msg_byteonly ()

let msg_no_dynlink_cmxa () =
  printf "Cannot find native-code dynlink library.\n"; msg_byteonly ();
  printf "For building a native-code Coq, you may try to first\n";
  printf "compile and install a dummy dynlink.cmxa (see dev/dynlink.ml)\n";
  printf "and then run ./configure -natdynlink no\n"

let check_native () =
  if !Prefs.byteonly then raise Not_found;
  if not (is_executable camlexec.opt || program_in_path camlexec.opt) then
    (msg_no_ocamlopt (); raise Not_found);
  if not (Sys.file_exists (fullcamlp4lib^"/"^camlp4mod^".cmxa")) then
    (msg_no_camlp4_cmxa (); raise Not_found);
  if not (Sys.file_exists (camllib^"/dynlink.cmxa")) then
    (msg_no_dynlink_cmxa (); raise Not_found);
  let version = run (camlexec.opt^" -version") in
  if version <> caml_version then
    printf
      "Warning: Native and bytecode compilers do not have the same version!\n";
  printf "You have native-code compilation. Good!\n"

let best_compiler =
  try check_native (); "opt" with Not_found -> "byte"


(** * Native dynlink *)

let hasnatdynlink = !Prefs.natdynlink && best_compiler = "opt"

(** OCaml 3.11.0 dynlink is buggy on MacOS 10.5, and possibly
    also on 10.6.(0|1|2) for x86_64 and 10.6.x on x86_32 *)

let needs_MacOS_fix () =
  match hasnatdynlink, arch, caml_version_nums with
  | true, "Darwin", 3::11::_ ->
    (match string_split '.' (run "uname -r") with
    | "9"::_ -> true
    | "10"::("0"|"1"|"2")::_ -> true
    | "10"::_ when Sys.word_size = 32 -> true
    | _ -> false)
  | _ -> false

let natdynlinkflag =
  if needs_MacOS_fix () then "os5fixme" else
    if hasnatdynlink then "true" else "false"


(** * OS dependent libraries *)

let osdeplibs = "-cclib -lunix"

let operating_system, osdeplibs =
  if starts_with arch "sun4" then
    let os = run "uname -r" in
    if starts_with os "5" then
      "Sun Solaris "^os, osdeplibs^" -cclib -lnsl -cclib -lsocket"
    else
      "Sun OS "^os, osdeplibs
  else
    "", osdeplibs


(** * lablgtk2 and CoqIDE *)

(** -byte-only implies -coqide byte, unless the user decides otherwise *)

let _ =
  if best_compiler = "byte" && !Prefs.coqide = None then
    Prefs.coqide := Some Byte

(** Which coqide is asked ? which one is possible ? *)

let check_lablgtkdir ?(fatal=false) msg dir =
  let yell msg = if fatal then die msg else (printf "%s\n" msg; false) in
  if not (dir_exists dir) then
    yell (sprintf "No such directory '%s' (%s)." dir msg)
  else if not (Sys.file_exists (dir^"/gSourceView2.cmi")) then
    yell (sprintf "Incomplete LablGtk2 (%s): no %s/gSourceView2.cmi." msg dir)
  else if not (Sys.file_exists (dir^"/glib.mli")) then
    yell (sprintf "Incomplete LablGtk2 (%s): no %s/glib.mli." msg dir)
  else true

let get_lablgtkdir () =
  match !Prefs.lablgtkdir with
  | Some dir ->
    let msg = "manually provided" in
    if check_lablgtkdir ~fatal:true msg dir then dir, msg
    else "", ""
  | None ->
    let msg = "via ocamlfind" in
    let d1 = tryrun "ocamlfind query lablgtk2.sourceview2 2> /dev/null" in
    if d1 <> "" && check_lablgtkdir msg d1 then d1, msg
    else
      (* In debian wheezy, ocamlfind knows only of lablgtk2 *)
      let d2 = tryrun "ocamlfind query lablgtk2 2> /dev/null" in
      if d2 <> "" && d2 <> d1 && check_lablgtkdir msg d2 then d2, msg
      else
        let msg = "in OCaml library" in
        let d3 = camllib^"/lablgtk2" in
        if check_lablgtkdir msg d3 then d3, msg
        else "", ""

let lablgtkdir = ref ""

let check_coqide () =
  (* If the user asks something impossible, we abort the configuration *)
  let check_expected res msg = match res, !Prefs.coqide with
    | No, Some (Byte|Opt) -> die msg
    | Byte, Some Opt -> die msg
    | _ -> let () = printf "%s\n" msg in res
  in
  match !Prefs.coqide with
  | Some No -> let () = printf "CoqIde disabled as requested.\n" in No
  | _ ->
    let dir, msg = get_lablgtkdir () in
    if dir = "" then
      check_expected No "LablGtk2 not found: CoqIde will not be available."
    else
      let msg1 = sprintf "LablGtk2 found (%s)" msg in
      let test = sprintf "grep -q -w convert_with_fallback %S/glib.mli" dir in
      if Sys.command test <> 0 then
        check_expected No (msg1 ^" but too old: CoqIde will not be available.")
      else
        let () = lablgtkdir := shorten_camllib dir in
        if !Prefs.coqide = Some Byte then
          let () = printf ", bytecode CoqIde will be used as requested.\n" in
          Byte
        else if not (Sys.file_exists (camllib^"/threads/threads.cmxa") &&
                     Sys.file_exists (dir^"/gtkThread.cmx"))
        then
          let msg2 = ", but no native LablGtk2 or threads:\n" in
          let msg3 = "only bytecode CoqIde will be available." in
          check_expected Byte (msg1^msg2^msg3)
        else
          let msg2 = ", native threads: native Coqide will be available.\n" in
          let () = printf "%s%s" msg1 msg2 in
          Opt

let coqide = check_coqide ()

let coqide_str = match coqide with Opt -> "opt" | Byte -> "byte" | No -> "no"

let lablgtkincludes = ref ""
let idearchflags = ref ""
let idearchfile = ref ""
let idearchdef = ref "X11"

let coqide_flags () =
  if !lablgtkdir <> "" then lablgtkincludes := sprintf "-I %S" !lablgtkdir;
  match coqide, arch with
    | Opt, "Darwin" when !Prefs.macintegration ->
      let osxdir = tryrun "ocamlfind query lablgtkosx 2> /dev/null" in
      if osxdir <> "" then begin
        lablgtkincludes := sprintf "%s -I %S" !lablgtkincludes osxdir;
        idearchflags := "lablgtkosx.cmxa";
        idearchdef := "QUARTZ"
      end
    | Opt, "win32" ->
      idearchfile := "ide/ide_win32_stubs.o";
      idearchdef := "WIN32"
    | _ -> ()

let _ = coqide_flags ()


(** * strip command *)

let strip =
  if arch = "Darwin" then
    if hasnatdynlink then "true" else "strip"
  else
    if !Prefs.profile || !Prefs.debug then "true" else "strip"


(** * Documentation : do we have latex, hevea, ... *)

let check_doc () =
  let err s =
    printf "%s was not found; documentation will not be available\n" s;
    raise Not_found
  in
  try
    if not !Prefs.withdoc then raise Not_found;
    if not (program_in_path "latex") then err "latex";
    if not (program_in_path "hevea") then err "hevea";
    true
  with Not_found -> false

let withdoc = check_doc ()


(*
###########################################
# bindir, libdir, mandir, docdir, etc.

COQTOP=$PWD

# OCaml only understand Windows filenames (C:\...)
case $ARCH in
    win32) COQTOP=`mk_win_path "$COQTOP"`
           CAMLBIN=`mk_win_path "$CAMLBIN"`
           CAMLP4BIN=`mk_win_path "$CAMLP4BIN"`
esac

# Default installation directories

case $ARCH$CYGWIN in
  win32)
         W32PREF='C:/coq/'
         bindir_def="${W32PREF}bin"
         libdir_def="${W32PREF}lib"
         configdir_def="${W32PREF}config"
         datadir_def="${W32PREF}share"
         mandir_def="${W32PREF}man"
         docdir_def="${W32PREF}doc"
         emacslib_def="${W32PREF}emacs"
         coqdocdir_def="${W32PREF}latex";;
  * )
         bindir_def=/usr/local/bin
         libdir_def=/usr/local/lib/coq
         configdir_def=/etc/xdg/coq
         datadir_def=/usr/local/share/coq
         mandir_def=/usr/local/share/man
         docdir_def=/usr/local/share/doc/coq
         emacslib_def=/usr/local/share/emacs/site-lisp
         coqdocdir_def=/usr/local/share/texmf/tex/latex/misc;;
esac

askdir () {
  printf "Where should I install $1 [%s]? " $2
  read answer
  if [ "$answer" = "" ]; then answer="$2"; fi
}

if [ $local = false ]; then

# Installation directories for a non-local build

case $bindir_spec/$prefix_spec in
    yes/* ) BINDIR=$bindir ;;
    no/yes) BINDIR=$prefix/bin ;;
    * ) askdir "the Coq binaries" $bindir_def
       BINDIR="$answer";;
esac

case $libdir_spec/$prefix_spec/$ARCH in
    yes/* ) LIBDIR=$libdir;;
    no/yes/win32) LIBDIR=$prefix;;
    no/yes/* ) LIBDIR=$prefix/lib/coq ;;
    * ) askdir "the Coq library" $libdir_def
       LIBDIR="$answer";;
esac
libdir_spec=yes

case $configdir_spec/$prefix_spec/$ARCH in
    yes/* ) CONFIGDIR=$configdir;;
    no/yes/win32) CONFIGDIR=$prefix/config;;
    no/yes/* ) CONFIGDIR=$prefix/etc/xdg/coq;;
    * ) askdir "the Coqide configuration files" $configdir_def
       CONFIGDIR="$answer";;
esac
if [ "$CONFIGDIR" != "$configdir_def" ]; then configdir_spec=yes; fi

case $datadir_spec/$prefix_spec in
    yes/* ) DATADIR=$datadir;;
    no/yes) DATADIR=$prefix/share/coq;;
    * ) askdir "the Coqide data files" $datadir_def
        DATADIR="$answer";;
esac
if [ "$DATADIR" != "datadir_def" ]; then datadir_spec=yes; fi

case $mandir_spec/$prefix_spec in
    yes/* ) MANDIR=$mandir;;
    no/yes) MANDIR=$prefix/share/man ;;
    * ) askdir "the Coq man pages" $mandir_def
       MANDIR="$answer";;
esac

case $docdir_spec/$prefix_spec in
    yes/* ) DOCDIR=$docdir;;
    no/yes) DOCDIR=$prefix/share/doc/coq;;
    * ) askdir "the Coq documentation [%s]? " $docdir_def
       DOCDIR="$answer";;
esac

case $emacslib_spec/$prefix_spec/$ARCH in
    yes/* ) EMACSLIB=$emacslib;;
    no/yes/win32) EMACSLIB=$prefix/emacs ;;
    no/yes/* ) EMACSLIB=$prefix/share/emacs/site-lisp ;;
    * ) askdir "the Coq Emacs mode" $emacslib_def
       EMACSLIB="$answer";;
esac

case $coqdocdir_spec/$prefix_spec/$ARCH in
    yes/* ) COQDOCDIR=$coqdocdir;;
    no/yes/win32) COQDOCDIR=$prefix/latex ;;
    no/yes/* ) COQDOCDIR=$prefix/share/emacs/site-lisp ;;
    * ) askdir "Coqdoc TeX/LaTeX files" $coqdocdir_def
       COQDOCDIR="$answer";;
esac

else # local build
    CONFIGDIR=$COQTOP/ide
    DATADIR=$COQTOP/ide
    configdir_spec=yes
    datadir_spec=yes
fi

# Determine if we enable -custom by default (Windows and MacOS)
CUSTOM_OS=no
if [ "$ARCH" = "win32" ] || [ "$ARCH" = "Darwin" ]; then
    CUSTOM_OS=yes
fi

BUILDLDPATH="# you might want to set CAML_LD_LIBRARY_PATH by hand!"
case $coqrunbyteflags_spec/$local/$custom_spec/$CUSTOM_OS in
    yes/*/*/* ) COQRUNBYTEFLAGS="$coqrunbyteflags";;
    */*/yes/*|*/*/*/yes) COQRUNBYTEFLAGS="-custom";;
    */true/*/* ) COQRUNBYTEFLAGS="-dllib -lcoqrun -dllpath '$COQTOP'/kernel/byterun";;
    * )
        COQRUNBYTEFLAGS="-dllib -lcoqrun -dllpath '$LIBDIR'"
        BUILDLDPATH="export CAML_LD_LIBRARY_PATH='$COQTOP'/kernel/byterun:$CAML_LD_LIBRARY_PATH";;
esac
case $coqtoolsbyteflags_spec/$custom_spec/$CUSTOM_OS in
    yes/*/* ) COQTOOLSBYTEFLAGS="$coqtoolsbyteflags";;
    */yes/*|*/*/yes) COQTOOLSBYTEFLAGS="-custom";;
    * ) COQTOOLSBYTEFLAGS="";;
esac
*)

(** * Summary of the configuration *)

let print_summary () =
  printf "\n";
  printf "  Architecture                      : %s\n" arch;
  if operating_system <> "" then
    printf "  Operating system                  : %s\n" operating_system;
(* TODO
  printf "  Coq VM bytecode link flags        : $COQRUNBYTEFLAGS\n";
  printf "  Coq tools bytecode link flags     : $COQTOOLSBYTEFLAGS\n";
  printf "  OS dependent libraries            : $OSDEPLIBS\n";
*)
  printf "  Objective-Caml/Camlp4 version     : %s\n" caml_version;
  printf "  Objective-Caml/Camlp4 binaries in : %s\n" camlbin;
  printf "  Objective-Caml library in         : %s\n" camllib;
  printf "  Camlp4 library in                 : %s\n" camlp4lib;
  if best_compiler = "opt" then
    printf "  Native dynamic link support       : %B\n" hasnatdynlink;
  if coqide <> No then
    printf "  Lablgtk2 library in               : %s\n" !lablgtkdir;
  if !idearchdef = "QUARTZ" then
    printf "  Mac OS integration is on\n";
  printf "  CoqIde                            : %s\n" coqide_str;
  printf "  Documentation                     : %s\n"
    (if withdoc then "All" else "None");
  printf "  Web browser                       : %s\n" browser;
  printf "  Coq web site                      : %s\n\n" !Prefs.coqwebsite;
  if not !Prefs.nativecompiler then
    printf "  Native compiler for conversion and normalization disabled\n\n";
  if !Prefs.local then
    printf "  Local build, no installation...\n"
  else begin
    printf "  Paths for true installation:\n";
    printf "    binaries      will be copied in $BINDIR\n";
    printf "    library       will be copied in $LIBDIR\n";
    printf "    config files  will be copied in $CONFIGDIR\n";
    printf "    data files    will be copied in $DATADIR\n";
    printf "    man pages     will be copied in $MANDIR\n";
    printf "    documentation will be copied in $DOCDIR\n";
    printf "    emacs mode    will be copied in $EMACSLIB\n";
  end;
  printf "\n"

let _ = print_summary ()


(** * Building the dev/ocamldebug-coq file *)

let ocamldebug_coq = "dev/ocamldebug-coq"

(*
if !Prefs.debug then begin
  Sys.remove ocamldebug_coq;
  let cmd = sprintf "sed -e 's|COQTOPDIRECTORY|%s|' -e 's|CAMLBINDIRECTORY|%s|' -e 's|CAMLP4LIBDIRECTORY|

  Sys.command
    ("sed -e 's|COQTOPDIRECTORY|"^$COQTOP|' "^
          "-e 's|CAMLBINDIRECTORY|$CAMLBIN|' "^
          "-e 's|CAMLP4LIBDIRECTORY|$FULLCAMLP4LIB|' " ^
      $OCAMLDEBUGCOQ.template > $OCAMLDEBUGCOQ
  chmod a-w,a+x $OCAMLDEBUGCOQ
fi


##############################################
# Creation of configuration files
##############################################

config_file=config/Makefile
config_template=config/Makefile.template


### Warning !!
### After this line, be careful when using variables,
### since some of them will be escaped

escape_string () {
    "$ocamlexec" "tools/escape_string.ml" "$1"
}

# Escaped version of browser command
BROWSER=`escape_string "$BROWSER"`

# Under Windows, we now escape the backslashes that will ends in
# ocaml strings (coq_config.ml) or in Makefile variables.

case $ARCH in
    win32)
        BINDIR=`escape_string "$BINDIR"`
        LIBDIR=`escape_string "$LIBDIR"`
        CONFIGDIR=`escape_string "$CONFIGDIR"`
        DATADIR=`escape_string "$DATADIR"`
        CAMLBIN=`escape_string "$CAMLBIN"`
        CAMLLIB=`escape_string "$CAMLLIB"`
        MANDIR=`escape_string "$MANDIR"`
        DOCDIR=`escape_string "$DOCDIR"`
        EMACSLIB=`escape_string "$EMACSLIB"`
        COQDOCDIR=`escape_string "$COQDOCDIR"`
        CAMLP4BIN=`escape_string "$CAMLP4BIN"`
        CAMLP4LIB=`escape_string "$CAMLP4LIB"`
        LABLGTKINCLUDES=`escape_string "$LABLGTKINCLUDES"`
        COQRUNBYTEFLAGS=`escape_string "$COQRUNBYTEFLAGS"`
        COQTOOLSBYTEFLAGS=`escape_string "$COQTOOLSBYTEFLAGS"`
        BUILDLDPATH=`escape_string "$BUILDLDPATH"`
        ocamlexec=`escape_string "$ocamlexec"`
        bytecamlc=`escape_string "$bytecamlc"`
        nativecamlc=`escape_string "$nativecamlc"`
        ocamlmklibexec=`escape_string "$ocamlmklibexec"`
        ocamldepexec=`escape_string "$ocamldepexec"`
        ocamldocexec=`escape_string "$ocamldocexec"`
        ocamllexexec=`escape_string "$ocamllexexec"`
        ocamlyaccexec=`escape_string "$ocamlyaccexec"`
        camlp4oexec=`escape_string "$camlp4oexec"`
    ;;
esac

case $libdir_spec in
    yes) LIBDIR_OPTION="Some \"$LIBDIR\"";;
    * ) LIBDIR_OPTION="None";;
esac

case $configdir_spec in
    yes) CONFIGDIR_OPTION="Some \"$CONFIGDIR\"";;
    * ) CONFIGDIR_OPTION="None";;
esac

case $datadir_spec in
    yes) DATADIR_OPTION="Some \"$DATADIR\"";;
    * ) DATADIR_OPTION="None";;
esac
*)

(** * Building the config/coq_config.ml file *)

let write_configml f my =
  Sys.remove f;
  let o = open_out f in
  let pr s = fprintf o s in
  let pr_s = pr "let %s = %S\n" in
  let pr_b = pr "let %s = %B\n" in
  let pr_i = pr "let %s = %d\n" in
  pr "(* DO NOT EDIT THIS FILE: automatically generated by ../configure *)\n";
  pr_b "local" !Prefs.local;
(*  fprintf o "let coqrunbyteflags = $COQRUNBYTEFLAGS\n";
  fprintf o "let coqlib = $LIBDIR_OPTION\n";
  fprintf o "let configdir = $CONFIGDIR_OPTION\n";
  fprintf o "let datadir = $DATADIR_OPTION\n";
  fprintf o "let docdir = "$DOCDIR"\n"; *)
  pr_s "ocaml" camlexec.top;
  pr_s "ocamlc" camlexec.byte;
  pr_s "ocamlopt" camlexec.opt;
  pr_s "ocamlmklib" camlexec.mklib;
  pr_s "ocamldep" camlexec.dep;
  pr_s "ocamldoc" camlexec.doc;
  pr_s "ocamlyacc" camlexec.yacc;
  pr_s "ocamllex" camlexec.lex;
  pr_s "camlbin" camlbin;
  pr_s "camllib" camllib;
  pr_s "camlp4" camlp4;
  pr_s "camlp4o" camlexec.p4;
  pr_s "camlp4bin" camlp4bin;
  pr_s "camlp4lib" camlp4lib;
  pr_s "camlp4compat" camlp4compat;
  pr_s "cflags" cflags;
  pr_s "best" best_compiler;
  pr_s "osdeplibs" osdeplibs;
  pr_s "version" coq_version;
  pr_s "caml_version" caml_version;
  pr_s "date" short_date;
  pr_s "compile_date" full_date;
  pr_s "arch" arch;
  pr_b "arch_is_win32" (arch = "win32");
  pr_s "exec_extension" exe;
  pr_s "coqideincl" !lablgtkincludes;
  pr_s "has_coqide" coqide_str;
  pr "let gtk_platform = `%s\n" !idearchdef;
  pr_b "has_natdynlink" hasnatdynlink;
  pr_s "natdynlinkflag" natdynlinkflag;
  pr_i "vo_magic_number" vo_magic;
  pr_i "state_magic_number" state_magic;
  pr "let with_geoproof = ref %B\n" !Prefs.geoproof;
  pr_s "browser" browser;
  pr_s "wwwcoq" !Prefs.coqwebsite;
  pr_s "wwwrefman" (!Prefs.coqwebsite ^ "distrib/" ^ coq_version ^ "/refman/");
  pr_s "wwwstdlib" (!Prefs.coqwebsite ^ "distrib/" ^ coq_version ^ "/stdlib/");
(* pr_s "localwwwrefman"  ("file:/" ^ docdir ^ "/html/refman"); *)
  pr_b "no_native_compiler" (not !Prefs.nativecompiler);
  pr "\nlet plugins_dirs = [\n";
  let plugins = Sys.readdir "plugins" in
  Array.sort compare plugins;
  Array.iter
    (fun f ->
      if Sys.is_directory ("plugins/"^f) && f.[0] <> '.' then pr "  %S;\n" f)
    plugins;
  pr "]\n";
  close_out o;
  Unix.chmod f 0o444;
  Sys.remove my;
  Unix.symlink f my

let _ = write_configml "config/coq_config.ml" "myocamlbuild_config.ml"


(** * Building the config/Makefile file *)

let write_makefile f =
  Sys.remove f;
  let o = open_out f in
  let pr s = fprintf o s in
  pr "###### config/Makefile : Configuration file for Coq ##############\n";
  pr "#                                                                #\n";
  pr "# This file is generated by the script \"configure\"               #\n";
  pr "# DO NOT EDIT IT !! DO NOT EDIT IT !! DO NOT EDIT IT !!          #\n";
  pr "# If something is wrong below, then rerun the script \"configure\" #\n";
  pr "# with the good options (see the file INSTALL).                  #\n";
  pr "#                                                                #\n";
  pr "##################################################################\n\n";
  pr "#Variable used to detect whether ./configure has run successfully.\n";
  pr "COQ_CONFIGURED=yes\n\n";
  pr "# Local use (no installation)\n";
  pr "LOCAL=%B\n\n" !Prefs.local;
  pr "# Bytecode link flags for VM (\"-custom\" or \"-dllib -lcoqrun\")\n";
(*TODO
COQRUNBYTEFLAGS=$COQRUNBYTEFLAGS
COQTOOLSBYTEFLAGS=$COQTOOLSBYTEFLAGS
$BUILDLDPATH

# Paths for true installation
# BINDIR=path where coqtop, coqc, coqmktop, coq-tex, coqdep, gallina and
#        do_Makefile will reside
# LIBDIR=path where the Coq library will reside
# MANDIR=path where to install manual pages
# EMACSDIR=path where to put Coq's Emacs mode (coq.el)
BINDIR="$BINDIR"
COQLIBINSTALL="$LIBDIR"
CONFIGDIR="$CONFIGDIR"
DATADIR="$DATADIR"
MANDIR="$MANDIR"
DOCDIR="$DOCDIR"
EMACSLIB="$EMACSLIB"
EMACS=$EMACS
*)
  pr "# Path to Coq distribution\n";
  pr "VERSION=%s\n\n" coq_version;
  pr "# Ocaml version number\n";
  pr "CAMLVERSION=%s\n\n" camltag;
  pr "# Ocaml libraries\n";
  pr "CAMLLIB=%S\n\n" camllib;
  pr "# Ocaml .h directory\n";
  pr "CAMLHLIB=%S\n\n" camllib;
  pr "# Camlp4 : flavor, binaries, libraries ...\n";
  pr "# NB : CAMLP4BIN can be empty if camlp4 is in the PATH\n";
  pr "# NB : avoid using CAMLP4LIB (conflict under Windows)\n";
  pr "CAMLP4BIN=%S\n" camlp4bin;
  pr "CAMLP4=%s\n" camlp4;
  pr "CAMLP4O=%S\n" camlexec.p4;
  pr "CAMLP4COMPAT=%S\n" camlp4compat;
  pr "MYCAMLP4LIB=%S\n\n" camlp4lib;
  pr "# LablGTK\n";
  pr "COQIDEINCLUDES=%S\n\n" !lablgtkincludes; (* TODO: check quotes *)
  pr "# Objective-Caml compile command\n";
  pr "OCAML=%S\n" camlexec.top;
  pr "OCAMLC=%S\n" camlexec.byte;
  pr "OCAMLMKLIB=%S\n" camlexec.mklib;
  pr "OCAMLOPT=%S\n" camlexec.opt;
  pr "OCAMLDEP=%S\n" camlexec.dep;
  pr "OCAMLDOC=%S\n" camlexec.doc;
  pr "OCAMLLEX=%S\n" camlexec.lex;
  pr "OCAMLYACC=%S\n\n" camlexec.yacc;
  pr "# Caml link command and Caml make top command\n";
  pr "CAMLLINK=%S\n" camlexec.byte;
  pr "CAMLOPTLINK=%S\n\n" camlexec.opt;
  pr "# Caml flags\n";
  pr "CAMLFLAGS=-rectypes %s\n" coq_annotate_flag;
  pr "TYPEREX=%s\n\n" coq_typerex_wrapper;
  pr "# Compilation debug flags\n";
  pr "CAMLDEBUG=%s\n" coq_debug_flag;
  pr "CAMLDEBUGOPT=%s\n\n" coq_debug_flag_opt;
  pr "# User compilation flag\n";
  pr "USERFLAGS=\n\n";
  pr "# Flags for GCC\n";
  pr "CFLAGS=%s\n\n" cflags;
  pr "# Compilation profile flag\n";
  pr "CAMLTIMEPROF=%s\n\n" coq_profile_flag;
  pr "# The best compiler: native (=opt) or bytecode (=byte)\n";
  pr "BEST=%s\n\n" best_compiler;
  pr "# Your architecture\n";
  pr "# Can be obtain by UNIX command arch\n";
  pr "ARCH=%s\n" arch;
  pr "HASNATDYNLINK=%s\n\n" natdynlinkflag;
  pr "# Supplementary libs for some systems, currently:\n";
  pr "#  . Sun Solaris: -cclib -lunix -cclib -lnsl -cclib -lsocket\n";
  pr "#  . others     : -cclib -lunix\n";
  pr "OSDEPLIBS=%s\n\n" osdeplibs;
  pr "# executable files extension, currently:\n";
  pr "#  Unix systems:\n";
  pr "#  Win32 systems : .exe\n";
  pr "EXE=%s\n" exe;
  pr "DLLEXT=%s\n\n" dll;
  pr "# the command MKDIR (try to use mkdirhier if you have problems)\n";
  pr "MKDIR=mkdir -p\n\n";
  pr "# where to put the coqdoc.sty style file\n";
(*  pr "COQDOCDIR=%s\n\n" coqdocdir; *)
  pr "#the command STRIP\n";
  pr "# Unix systems and profiling: true\n";
  pr "# Unix systems and no profiling: strip\n";
  pr "STRIP=%s\n\n" strip;
  pr "# CoqIde (no/byte/opt)\n";
  pr "HASCOQIDE=%s\n" coqide_str;
  pr "IDEOPTFLAGS=%s\n" !idearchflags;
  pr "IDEOPTDEPS=%s\n" !idearchfile;
  pr "IDEOPTINT=%s\n\n" !idearchdef;
  pr "# Defining REVISION\n";
  pr "CHECKEDOUT=%s\n\n" vcs;
  pr "# Option to control compilation and installation of the documentation\n";
  pr "WITHDOC=%s\n" (if withdoc then "all" else "no");
  close_out o;
  Unix.chmod f 0o444

let _ = write_makefile "config/Makefile"


(** * The end *)

let final_message () =
  printf "If anything in the above is wrong, please restart './configure'.\n";
  printf "\n";
  printf "*Warning* To compile the system for a new architecture\n";
  printf "          don't forget to do a 'make clean' before './configure'.\n"

let _ = final_message ()
