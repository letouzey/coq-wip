(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** From Lambda code to Bytecode *)

let reset_compiler () =
  Translobj.reset_labels ();
  Translmod.primitive_declarations := [];
  Env.reset_cache ()

(** Compile a standalone lambda expression to a cmo.
    This lambda must be a block with all the toplevel definitions.
    Code borrowed from drivers/compile.ml *)

let compile_lambda ?(debug=false) modulename lam =
  let mod_id = Ident.create_persistent modulename in
  let lambda = Lambda.Lprim (Lambda.Psetglobal mod_id,[lam]) in
  let lambda' = Simplif.simplify_lambda lambda in
  if debug then Printlambda.lambda Format.std_formatter lambda';
  let bytecode = Bytegen.compile_implementation modulename lambda' in
  if debug then Printinstr.instrlist Format.std_formatter bytecode;
  let oc = open_out_bin (modulename^".cmo") in
  let () = Emitcode.to_file oc modulename bytecode in
  close_out oc


(** Compile, link and execute a lambda phrase.
    Code borrowed from toplevel/toploop.ml *)

let eval_lambda ?(debug=false) lam =
  let slam = Simplif.simplify_lambda lam in
  if debug then Printlambda.lambda Format.std_formatter slam;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  try
    let retval = (Meta.reify_bytecode code code_size) () in
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    retval
  with x ->
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Symtable.restore_state initial_symtable;
    raise x
