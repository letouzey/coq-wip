
type res = Ok of Obj.t | Err of string
type evaluation_outcome = Result of Obj.t | Exception of exn

external ndl_run_toplevel: string -> string -> res
  = "caml_natdynlink_run_toplevel"
external ndl_loadsym: string -> Obj.t = "caml_natdynlink_loadsym"

let dll_run dll entry =
  match (try Result (Obj.magic (ndl_run_toplevel dll entry))
    with exn -> Exception exn)
  with
  | Exception _ as r -> r
  | Result r ->
        match Obj.magic r with
        | Ok x -> Result x
        | Err s -> failwith ("Opttoploop.dll_run " ^ s)

(* Load in-core and execute a lambda term *)

(* Mettre un gensym !!
      phrase_name := Printf.sprintf "TOP%i" !phrase_seqid;
      Compilenv.reset ?packname:None !phrase_name;
*)

let ext_dll = ".so"
let ext_obj = ".o"

let phrase_seqid = ref 0
let phrase_name = ref "TOP"

let need_symbol sym =
  try ignore (ndl_loadsym sym); false
  with _ -> true

let eval_lambda ?(debug=false) lam =
  Compilenv.reset ?packname:None !phrase_name;
  let size = 1 in (* TODO !! *)
  let ppf = Format.std_formatter in
  let slam = Simplif.simplify_lambda lam in
  if debug then Printlambda.lambda ppf slam;
  let dll = Filename.temp_file ("caml" ^ !phrase_name) ext_dll in
  let fn = Filename.chop_extension dll in
  Asmgen.compile_implementation ~toplevel:need_symbol fn ppf (size, lam);
  Asmlink.call_linker_shared [fn ^ ext_obj] dll;
  Sys.remove (fn ^ ext_obj);
  let dll =
    if Filename.is_implicit dll
    then Filename.concat (Sys.getcwd ()) dll
    else dll in
  let res = dll_run dll !phrase_name in
  (try Sys.remove dll with Sys_error _ -> ());
  (* note: under windows, cannot remove a loaded dll
     (should remember the handles, close them in at_exit, and then remove
     files) *)
  match res with
  | Result o -> o
  | Exception e -> raise e
