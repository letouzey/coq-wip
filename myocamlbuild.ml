open Ocamlbuild_plugin
open Ocamlbuild_pack

let mlflags = S [A"-rectypes"; A"-I"; A "+camlp5"]
let cflags = S [A"-ccopt"; A"-fno-defer-pop -Wall -Wno-unused"]
let c_headers_base =
  ["coq_fix_code.h";"coq_instruct.h"; "coq_memory.h"; "int64_emul.h";
   "coq_gc.h"; "coq_interp.h"; "coq_values.h"; "int64_native.h";
   "coq_jumptbl.h"]
let c_headers = List.map ((^) "kernel/byterun/") c_headers_base
let coqrunbyteflags =
  "-dllib -lcoqrun -dllpath /home/letouzey/V8/_build/kernel/byterun"
let libcoqrun = "kernel/byterun/libcoqrun.a"

let _ =
  ocaml_lib ~extern:true "gramlib";
  flag ["compile"; "ocaml"] mlflags;
  flag ["link"; "ocaml"] mlflags;
  flag ["pack"; "ocaml"] mlflags;

  dep ["compile"; "c"] c_headers;
  flag ["compile"; "c"] cflags;
  dep ["ocaml"; "use_libcoqrun"; "compile"] [libcoqrun];
  dep ["ocaml"; "use_libcoqrun"; "link"; "native"] [libcoqrun];
  flag ["ocaml"; "use_libcoqrun"; "link"; "byte"] (Sh coqrunbyteflags);

(*TODO : regarder ces .mldyl *)


(*
let extra_rules () = begin

end

(** Registration of our rules (after the standard ones) *)

let _ = dispatch begin function
  | After_rules -> initial_actions (); extra_rules ()
  | _ -> ()
end
*)
