(**********************************)

(**  Configuration script for Coq *)

(**********************************)

(** This file should be run via: ocaml configure.ml <opts> *)

#load "unix.cma"
open Printf

let _ = Array.iter (fun s -> print_string s; print_newline ()) Sys.argv;;

let coq_version = "trunk"
let vo_magic = "08511"
let state_magic = "58511"

(** * Utility functions *)

let die msg =
  eprintf "%s\nConfiguration script failed!\n" msg; exit 1

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

(* TODO: .exe on win32 ? *)

let which ?(path=global_path) prog =
  let rec search = function
    | [] -> raise Not_found
    | dir :: path ->
      let file = if dir = "" then "./"^prog else dir^"/"^prog in
      if is_executable file then file else search path
  in search path

(* TODO: useless ?
let program_in_path prog =
  try let _ = which prog in true with Not_found -> false
*)

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
    let arch = run ~fatal:false (prog^opt) in
    if arch <> "" then arch else try_archs rest
  | _ :: rest -> try_archs rest
  | [] -> query_arch ()

let cygwin = ref false

let arch = match !Prefs.arch with
  | Some a -> a
  | None ->
    let arch = run ~fatal:false "uname -s" in
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

(** Turn a [caml_version] string such as "4.01.0+rc2" into the list
    ["4";"01";"1"] *)

let caml_version_list =
  let isnum c = (c = '.' || (c >= '0' && c <= '9')) in
  let max = String.length caml_version in
  let i = ref 0 in
  while !i < max && isnum caml_version.[!i] do incr i done;
  string_split '.' (String.sub caml_version 0 !i)

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


(** * Camlp4 / Camlp5 configuration *)

(** We assume that camlp(4|5) binaries are at the same place as ocaml ones
    (this should become configurable some day). *)

let camlp4bin = camlbin

let camlp4 = ref true
let camlp4mod = ref ""

(* TODO: camlp5dir should rather be the *binary* location *)

(*
let camlp4lib, fullcamlp4lib =
  if !Prefs.usecamlp5 then begin
    camlp4 := false;
    camlp4mod := "gramlib";
    let testcma = !camlp4mod^".cma" in
    match !Prefs.camlp5dir with
    | Some dir ->
      if Sys.file_exists (dir^"/"^ testcma) then
        dir, dir
      else
        let msg =
          sprintf "Cannot find camlp5 libraries in '%s' (%s not found)."
            dir testcma
        in die msg
    | None ->
      let dir = run ~fatal:false "camlp5 -where" in
      if dir <> "" then
        dir, dir
      else if Sys.file_exists (camllib^"/camlp5/"^ testcma) then
        "+camlp5", (camllib^"/camlp5")
      else if Sys.file_exists (camllib^"/site-lib/camlp5/"^ testcma) then
        "+site-lib/camlp5", (camllib^"site-lib/camlp5")
      else
        begin
	  printf "No Camlp5 installation found. Looking for Camlp4 instead...\n";
          camlp4 := true;

        end
  end


(* If we're going to use Camlp5, let's check its version *)

let _ =
  if !camlp4 = "camlp5" then begin

  end
    camlp4oexec=`printf "$camlp4oexec" | tr 4 5`
    case `"$camlp4oexec" -v 2>&1` in
        *"version 4.0"*|*5.00* )
            printf "Camlp5 version < 5.01 not supported."
            printf "Configuration script failed!"
            exit 1;;
    esac
esac

(** We might now try to use Camlp4, either by explicit choice or
    by lack of proper Camlp5 installation *)

case $usecamlp5 in
  no)
    CAMLP4=camlp4
    CAMLP4MOD=camlp4lib
    CAMLP4LIB=+camlp4
    FULLCAMLP4LIB=${CAMLLIB}/camlp4

    if [ ! -f "${FULLCAMLP4LIB}/${CAMLP4MOD}.cma" ]; then
        printf "No Camlp4 installation found."
        printf "Configuration script failed!"
        exit 1
    fi

    camlp4oexec=${camlp4oexec}rf
    if [ "`"$camlp4oexec" 2>&1`" != "" ]; then
        printf "Error: $camlp4oexec not found or not executable."
        printf "Configuration script failed!"
        exit 1
    fi
esac

# do we have a native compiler: test of ocamlopt and its version

if [ "$best_compiler" = "opt" ] ; then
  if test -e "$nativecamlc" || test -e "`which $nativecamlc`"; then
      CAMLOPTVERSION=`"$nativecamlc" -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
      if [ ! -f "${FULLCAMLP4LIB}/${CAMLP4MOD}.cmxa" ]; then
          best_compiler=byte
          printf "Cannot find native-code $CAMLP4,"
          printf "only the bytecode version of Coq will be available."
      elif [ ! -f "$CAMLLIB"/dynlink.cmxa ]; then
          best_compiler=byte
          printf "Cannot find native-code dynlink library,"
          printf "only the bytecode version of Coq will be available."
          printf "For building a native-code Coq, you may try to first"
          printf "compile and install a dummy dynlink.cmxa (see dev/dynlink.ml)"
          printf "and then run ./configure -natdynlink no"
      else
          if [ "$CAMLOPTVERSION" != "$CAMLVERSION" ] ; then
              printf "Native and bytecode compilers do not have the same version!"
          fi
          printf "You have native-code compilation. Good!"
      fi
  else
      best_compiler=byte
      printf "You have only bytecode compilation."
  fi
fi

# Native dynlink
if [ "$natdynlink" = "yes" -a -f "$CAMLLIB"/dynlink.cmxa ]; then
    HASNATDYNLINK=true
else
    HASNATDYNLINK=false
fi

case $HASNATDYNLINK,$ARCH,`uname -r`,$CAMLVERSION in
    true,Darwin,9.*,3.11.* )  # ocaml 3.11.0 dynlink on MacOS 10.5 is buggy
        NATDYNLINKFLAG=os5fixme;;
    #Possibly a problem on 10.6.0/10.6.1/10.6.2
    #May just be a 32 vs 64 problem for all 10.6.*
    true,Darwin,10.0.*,3.11.* ) # Possibly a problem on 10.6.0
        NATDYNLINKFLAG=os5fixme;;
    true,Darwin,10.1.*,3.11.* ) # Possibly a problem on 10.6.1
        NATDYNLINKFLAG=os5fixme;;
    true,Darwin,10.2.*,3.11.* ) # Possibly a problem on 10.6.2
        NATDYNLINKFLAG=os5fixme;;
    true,Darwin,10.*,3.11.* )
        if [ `getconf LONG_BIT` = "32" ]; then
            # Still a problem for x86_32
            NATDYNLINKFLAG=os5fixme
        else
            # Not a problem for x86_64
            NATDYNLINKFLAG=$HASNATDYNLINK
        fi;;
    * )
        NATDYNLINKFLAG=$HASNATDYNLINK;;
esac
*)

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

(*
# lablgtk2 and CoqIDE

IDEARCHFLAGS=
IDEARCHFILE=
IDEARCHDEF=X11

# -byte-only should imply -coqide byte, unless the user decides otherwise

if [ "$best_compiler" = "byte" -a "$coqide_spec" = "no" ]; then
    coqide_spec=yes
    COQIDE=byte
fi

# Which coqide is asked ? which one is possible ?

if [ "$coqide_spec" = "yes" -a "$COQIDE" = "no" ]; then
    printf "CoqIde disabled as requested."
else
    case $lablgtkdir_spec in
        no)
            # Beware of the final \r in Win32
            lablgtkdirtmp="$(ocamlfind query lablgtk2.sourceview2 2> /dev/null | tr -d '\r')"
            if [ "$lablgtkdirtmp" != "" ]; then
                if [ ! -f "$lablgtkdirtmp/gSourceView2.cmi" ]; then
                    printf "Incomplete Lablgtk2 found by ocamlfind (gSourceView2.cmi not found)."
                elif [ ! -f "$lablgtkdirtmp/glib.mli" ]; then
                    printf "Incomplete Lablgtk2 found by ocamlfind (glib.mli not found)."
                else
                    lablgtkdirfoundmsg="LabelGtk2 found by ocamlfind"
                    lablgtkdir=$lablgtkdirtmp
                    LABLGTKLIB=$lablgtkdir # Pour le message utilisateur
                fi
            fi
            if [ "$lablgtkdir" = "" -a -f "${CAMLLIB}/lablgtk2/gSourceView2.cmi" -a -f "${CAMLLIB}/lablgtk2/glib.mli" ]; then
                lablgtkdirfoundmsg="LablGtk2 found in ocaml lib directory"
                lablgtkdir=${CAMLLIB}/lablgtk2
                LABLGTKLIB=+lablgtk2 # Pour le message utilisateur
            fi;;
        yes)
            if [ ! -d "$lablgtkdir" ]; then
                printf "$lablgtkdir is not a valid directory."
                printf "Configuration script failed!"
                exit 1
            elif [ ! -f "$lablgtkdir/gSourceView2.cmi" ]; then
                printf "Incomplete LablGtk2 library (gSourceView2.cmi not found)."
                printf "Make sure that the GtkSourceView bindings are available."
                printf "Configuration script failed!"
                exit 1
            elif [ ! -f "$lablgtkdir/glib.mli" ]; then
                printf "Incomplete LablGtk2 library (glib.mli not found)."
                printf "Configuration script failed!"
                exit 1
            else
                lablgtkdirfoundmsg="LablGtk2 directory found"
                LABLGTKLIB=$lablgtkdir # Pour le message utilisateur
            fi;;
    esac
    if [ "$lablgtkdir" = "" ]; then
        printf "LablGtk2 not found: CoqIde will not be available."
        COQIDE=no
    elif [ -z "`grep -w convert_with_fallback "$lablgtkdir/glib.mli"`" ]; then
        printf "$lablgtkdirfoundmsg but too old: CoqIde will not be available."
        COQIDE=no;
    elif [ "$coqide_spec" = "yes" -a "$COQIDE" = "byte" ]; then
        printf "$lablgtkdirfoundmsg, bytecode CoqIde will be used as requested."
        COQIDE=byte
    elif [ ! -f "${CAMLLIB}/threads/threads.cmxa" -a -f "${lablgtkdir}/gtkThread.cmx" ]; then
        printf "$lablgtkdirfoundmsg, not native (or no native threads): bytecode CoqIde will be available."
        COQIDE=byte
    else
        printf "$lablgtkdirfoundmsg, native threads: native CoqIde will be available."
        COQIDE=opt
        if [ "$nomacintegration_spec" = "no" ] && lablgtkosxdir=$(ocamlfind query lablgtkosx 2> /dev/null);
        then
            IDEARCHFLAGS=lablgtkosx.cmxa
            IDEARCHDEF=QUARTZ
        elif [ "$ARCH" = "win32" ];
        then
            IDEARCHFLAGS=
            IDEARCHFILE=ide/ide_win32_stubs.o
            IDEARCHDEF=WIN32
        fi
    fi
fi

case $COQIDE in
    byte|opt)
        LABLGTKINCLUDES="-I $LABLGTKLIB";;
    no)
        LABLGTKINCLUDES="";;
esac

[ x$lablgtkosxdir = x ] || LABLGTKINCLUDES="$LABLGTKINCLUDES -I $lablgtkosxdir"

# strip command

case $ARCH in
    Darwin) if [ "$HASNATDYNLINK" = "true" ]
        then
          STRIPCOMMAND="true"
        else
          STRIPCOMMAND="strip"
        fi;;
    * )
    if [ "$coq_profile_flag" = "-p" ] || [ "$coq_debug_flag" = "-g" ]
    then
        STRIPCOMMAND="true"
    else
        STRIPCOMMAND="strip"
    fi
esac

### Test if documentation can be compiled (latex, hevea)

if test "$with_doc" = "all"
then
    for cmd in "latex" "hevea" ; do
        if test ! -x "`which $cmd`"
        then
            printf "$cmd was not found; documentation will not be available"
            with_doc=no
            break
        fi
    done
fi

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
  printf "  Coq VM bytecode link flags        : $COQRUNBYTEFLAGS\n";
  printf "  Coq tools bytecode link flags     : $COQTOOLSBYTEFLAGS\n";
  printf "  OS dependent libraries            : $OSDEPLIBS\n";
  printf "  Objective-Caml/Camlp4 version     : $CAMLVERSION\n";
  printf "  Objective-Caml/Camlp4 binaries in : $CAMLBIN\n";
  printf "  Objective-Caml library in         : $CAMLLIB\n";
  printf "  Camlp4 library in                 : $CAMLP4LIB\n";
(*
if test "$best_compiler" = opt ; then
printf "  Native dynamic link support       : $HASNATDYNLINK"
fi
if test "$COQIDE" != "no"; then
printf "  Lablgtk2 library in               : $LABLGTKLIB"
fi
if test "$IDEARCHDEF" = "QUARTZ"; then
printf "  Mac OS integration is on"
fi
*)
  printf "  Documentation                     : %s\n"
    (if !Prefs.withdoc then "All" else "None");
  printf "  CoqIde                            : $COQIDE\n";
  printf "  Web browser                       : %s\n" browser;
  printf "  Coq web site                      : %s\n" !Prefs.coqwebsite;
  printf "\n";

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

mlconfig_file=config/coq_config.ml
mymlconfig_file=myocamlbuild_config.ml
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

#####################################################
# Building the config/coq_config.ml file
#####################################################

rm -f "$mlconfig_file" "$mymlconfig_file"
cat << END_OF_COQ_CONFIG > $mlconfig_file
(* DO NOT EDIT THIS FILE: automatically generated by ../configure *)

let local = $local
let coqrunbyteflags = "$COQRUNBYTEFLAGS"
let coqlib = $LIBDIR_OPTION
let configdir = $CONFIGDIR_OPTION
let datadir = $DATADIR_OPTION
let docdir = "$DOCDIR"
let ocaml = "$ocamlexec"
let ocamlc = "$bytecamlc"
let ocamlopt = "$nativecamlc"
let ocamlmklib = "$ocamlmklibexec"
let ocamldep = "$ocamldepexec"
let ocamldoc = "$ocamldocexec"
let ocamlyacc = "$ocamlyaccexec"
let ocamllex = "$ocamllexexec"
let camlbin = "$CAMLBIN"
let camllib = "$CAMLLIB"
let camlp4 = "$CAMLP4"
let camlp4o = "$camlp4oexec"
let camlp4bin = "$CAMLP4BIN"
let camlp4lib = "$CAMLP4LIB"
let camlp4compat = "$CAMLP4COMPAT"
let coqideincl = "$LABLGTKINCLUDES"
let cflags = "$cflags"
let best = "$best_compiler"
let arch = "$ARCH"
let arch_is_win32 = $ARCH_WIN32
let has_coqide = "$COQIDE"
let gtk_platform = \`$IDEARCHDEF
let has_natdynlink = $HASNATDYNLINK
let natdynlinkflag = "$NATDYNLINKFLAG"
let osdeplibs = "$OSDEPLIBS"
let version = "$VERSION"
let caml_version = "$CAMLVERSION"
let date = "$DATE"
let compile_date = "$COMPILEDATE"
let vo_magic_number = $VOMAGIC
let state_magic_number = $STATEMAGIC
let exec_extension = "$EXE"
let with_geoproof = ref $with_geoproof
let browser = "$BROWSER"
let wwwcoq = "$WWWCOQ"
let wwwrefman = wwwcoq ^ "distrib/" ^ version ^ "/refman/"
let wwwstdlib = wwwcoq ^ "distrib/" ^ version ^ "/stdlib/"
let localwwwrefman = "file:/" ^ docdir ^ "html/refman"
let no_native_compiler = $no_native_compiler


END_OF_COQ_CONFIG

printf "let plugins_dirs = [" >> "$mlconfig_file"
find plugins/* \( -name .svn -prune \) -o \( -type d -exec printf "\"%s\";\n" {} \; \) >> "$mlconfig_file"
printf "]" >> "$mlconfig_file"

chmod a-w "$mlconfig_file"
ln -sf "$mlconfig_file" "$mymlconfig_file"

###############################################
# Building the config/Makefile file
###############################################

rm -f "$config_file"

cat << END_OF_MAKEFILE > $config_file
###### config/Makefile : Configuration file for Coq ##############
#                                                                #
# This file is generated by the script "configure"               #
# DO NOT EDIT IT !! DO NOT EDIT IT !! DO NOT EDIT IT !!          #
# If something is wrong below, then rerun the script "configure" #
# with the good options (see the file INSTALL).                  #
#                                                                #
##################################################################

#Variable used to detect whether ./configure has run successfully.
COQ_CONFIGURED=yes

# Local use (no installation)
LOCAL=$local

# Bytecode link flags for VM ("-custom" or "-dllib -lcoqrun")
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

# Path to Coq distribution
VERSION=$VERSION

# Ocaml version number
CAMLVERSION=$CAMLTAG

# Ocaml libraries
CAMLLIB="$CAMLLIB"

# Ocaml .h directory
CAMLHLIB="$CAMLLIB"

# Camlp4 : flavor, binaries, libraries ...
# NB : CAMLP4BIN can be empty if camlp4 is in the PATH
# NB : avoid using CAMLP4LIB (conflict under Windows)
CAMLP4BIN="$CAMLP4BIN"
CAMLP4=$CAMLP4
CAMLP4O=$camlp4oexec
CAMLP4COMPAT=$CAMLP4COMPAT
MYCAMLP4LIB="$CAMLP4LIB"

# LablGTK
COQIDEINCLUDES=$LABLGTKINCLUDES

# Objective-Caml compile command
OCAML="$ocamlexec"
OCAMLC="$bytecamlc"
OCAMLMKLIB="$ocamlmklibexec"
OCAMLOPT="$nativecamlc"
OCAMLDEP="$ocamldepexec"
OCAMLDOC="$ocamldocexec"
OCAMLLEX="$ocamllexexec"
OCAMLYACC="$ocamlyaccexec"

# Caml link command and Caml make top command
CAMLLINK="$bytecamlc"
CAMLOPTLINK="$nativecamlc"

# Caml flags
CAMLFLAGS=-rectypes $coq_annotate_flag
TYPEREX=$coq_typerex_wrapper

# Compilation debug flags
CAMLDEBUG=$coq_debug_flag
CAMLDEBUGOPT=$coq_debug_flag_opt

# User compilation flag
USERFLAGS=

# Flags for GCC
CFLAGS=$cflags

# Compilation profile flag
CAMLTIMEPROF=$coq_profile_flag

# The best compiler: native (=opt) or bytecode (=byte) if no native compiler
BEST=$best_compiler

# Your architecture
# Can be obtain by UNIX command arch
ARCH=$ARCH
HASNATDYNLINK=$NATDYNLINKFLAG

# Supplementary libs for some systems, currently:
#  . Sun Solaris: -cclib -lunix -cclib -lnsl -cclib -lsocket
#  . others     : -cclib -lunix
OSDEPLIBS=$OSDEPLIBS

# executable files extension, currently:
#  Unix systems:
#  Win32 systems : .exe
EXE=$EXE
DLLEXT=$DLLEXT

# the command MKDIR (try to replace it with mkdirhier if you have problems)
MKDIR=mkdir -p

# where to put the coqdoc.sty style file
COQDOCDIR="$COQDOCDIR"

#the command STRIP
# Unix systems and profiling: true
# Unix systems and no profiling: strip
STRIP=$STRIPCOMMAND

# CoqIde (no/byte/opt)
HASCOQIDE=$COQIDE
IDEOPTFLAGS=$IDEARCHFLAGS
IDEOPTDEPS=$IDEARCHFILE
IDEOPTINT=$IDEARCHDEF

# Defining REVISION
CHECKEDOUT=$vcs

# Option to control compilation and installation of the documentation
WITHDOC=$with_doc

# make or sed are bogus and believe lines not terminating by a return
# are inexistent
END_OF_MAKEFILE

chmod a-w "$config_file"
*)

(** * The end *)

let final_message () =
  printf "If anything in the above is wrong, please restart './configure'.\n";
  printf "\n";
  printf "*Warning* To compile the system for a new architecture\n";
  printf "          don't forget to do a 'make clean' before './configure'.\n"

let _ = final_message ()
