(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

{
  open Lexing

  (* Without this table, the automaton would be too big and
     ocamllex would fail *)

  let tag_of_ident =
    let one_word_commands =
      [ "Add" ; "Check"; "Eval"; "Extraction" ;
	"Load" ; "Undo"; "Goal";
	"Proof" ; "Print";"Save" ; "Restart";
	"End" ; "Section"; "Chapter"; "Transparent"; "Opaque"; "Comments" ]
    in
    let one_word_decls =
      [ (* Definitions *)
	"Definition" ; "Let" ; "Example" ; "SubClass" ;
        "Fixpoint" ; "CoFixpoint" ; "Scheme" ; "Function" ;
        (* Assumptions *)
	"Hypothesis" ; "Variable" ; "Axiom" ; "Parameter" ; "Conjecture" ;
	"Hypotheses" ; "Variables" ; "Axioms" ; "Parameters";
        (* Inductive *)
        "Inductive" ; "CoInductive" ; "Record" ; "Structure" ;
        (* Other *)
	"Ltac" ; "Instance"; "Include"; "Context"; "Class" ;
	 "Arguments" ]
    in
    let proof_decls =
      [ "Theorem" ; "Lemma" ; " Fact" ; "Remark" ; "Corollary" ;
        "Proposition" ; "Property" ]
    in
    let proof_ends =
      [ "Qed" ; "Defined" ; "Admitted"; "Abort" ]
    in
    let constr_keywords =
      [ "forall"; "fun"; "match"; "fix"; "cofix"; "with"; "for";
	"end"; "as"; "let"; "in"; "if"; "then"; "else"; "return";
	"Prop"; "Set"; "Type" ]
    in
    let h = Hashtbl.create 97 in (* for vernac *)
    let h' = Hashtbl.create 97 in (* for constr *)
    List.iter (fun s -> Hashtbl.add h s Tags.Script.kwd) one_word_commands;
    List.iter (fun s -> Hashtbl.add h s Tags.Script.decl) one_word_decls;
    List.iter (fun s -> Hashtbl.add h s Tags.Script.proof_decl) proof_decls;
    List.iter (fun s -> Hashtbl.add h s Tags.Script.qed) proof_ends;
    List.iter (fun s -> Hashtbl.add h' s Tags.Script.kwd) constr_keywords;
    (fun initial id -> Hashtbl.find (if initial then h else h') id)

  exception Unterminated

  let here f mkiter lexbuf =
    let start_it = mkiter (Lexing.lexeme_start lexbuf) in
    let stop_it = mkiter (Lexing.lexeme_end lexbuf) in
    f start_it stop_it

}

let space =
  [' ' '\n' '\r' '\t' '\012'] (* '\012' is form-feed *)

let firstchar =
  ['$' 'A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identchar =
  ['$' 'A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let ident = firstchar identchar*

let undotted_sep = [ '{' '}' '-' '+' '*' ]

let dot_sep = '.' (space | eof)

let multiword_declaration =
  "Module" (space+ "Type")?
| "Program" space+ ident
| "Existing" space+ "Instance" "s"?
| "Canonical" space+ "Structure"

let locality = (space+ "Local")?

let multiword_command =
  ("Uns" | "S")" et" (space+ ident)*
| (("Open" | "Close") locality | "Bind" | " Delimit" )
    space+ "Scope"
| (("Reserved" space+)? "Notation" | "Infix") locality space+
| "Next" space+ "Obligation"
| "Solve" space+ "Obligations"
| "Require" space+ ("Import"|"Export")?
| "Hint" locality space+ ident
| "Reset" (space+ "Initial")?
| "Tactic" space+ "Notation"
| "Implicit" space+ "Type" "s"?
| "Combined" space+ "Scheme"
| "Extraction" space+ (("Language" space+ ("Ocaml"|"Haskell"|"Scheme"|"Toplevel"))|
    ("Library"|"Inline"|"NoInline"|"Blacklist"))
| "Recursive" space+ "Extraction" (space+ "Library")?
| ("Print"|"Reset") space+ "Extraction" space+ ("Inline"|"Blacklist")
| "Extract" space+ (("Inlined" space+) "Constant"| "Inductive")
| "Typeclasses" space+ ("eauto" | "Transparent" | "Opaque")
| ("Generalizable" space+) ("All" | "No")? "Variable" "s"?

(* At least still missing: "Inline" + decl, variants of "Identity
  Coercion", variants of Print, Add, ... *)

rule coq_string = parse
  | "\"\"" { coq_string lexbuf }
  | "\"" { () }
  | eof { () }
  | _ { coq_string lexbuf }

and comment = parse
  | "(*" { ignore (comment lexbuf); comment lexbuf }
  | "\"" { coq_string lexbuf; comment lexbuf }
  | "*)" { (true, Lexing.lexeme_end lexbuf) }
  | eof { (false, Lexing.lexeme_end lexbuf) }
  | _ { comment lexbuf }

(** NB : [mkiter] should be called on increasing offsets *)

and sentence initial stamp mkiter = parse
  | "(*" {
      let comm_start = Lexing.lexeme_start lexbuf in
      let trully_terminated,comm_end = comment lexbuf in
      let start = mkiter comm_start in
      let stop = mkiter comm_end in
      stamp start stop Tags.Script.comment;
      if not trully_terminated then raise Unterminated;
      (* A comment alone is a sentence.
	 A comment in a sentence doesn't terminate the sentence.
         Note: stop is the first position _after_ the comment,
	 as required when tagging a zone. *)
      if initial then begin
	stamp stop#backward_char stop Tags.Script.sentence;
	sentence true stamp mkiter lexbuf
      end else begin
	sentence false stamp mkiter lexbuf
      end
    }
  | "\"" {
      coq_string lexbuf;
      sentence false stamp mkiter lexbuf
    }
  | multiword_declaration {
      if initial then here stamp mkiter lexbuf Tags.Script.decl;
      sentence false stamp mkiter lexbuf
    }
  | multiword_command {
      if initial then here stamp mkiter lexbuf Tags.Script.kwd;
      sentence false stamp mkiter lexbuf
    }
  | ident as id {
      (try here stamp mkiter lexbuf (tag_of_ident initial id)
       with Not_found -> ());
      sentence false stamp mkiter lexbuf }
  | ".." {
      (* We must have a particular rule for parsing "..", where no dot
	 is a terminator, even if we have a blank afterwards
	 (cf. for instance the syntax for recursive notation).
	 This rule and the following one also allow to treat the "..."
	 special case, where the third dot is a terminator. *)
      sentence false stamp mkiter lexbuf
    }
  | dot_sep {
      (* the usual "." terminator *)
      let start = mkiter (Lexing.lexeme_start lexbuf) in
      stamp start start#forward_char Tags.Script.sentence;
      sentence true stamp mkiter lexbuf
    }
  | undotted_sep {
      (* Separators like { or } and bullets * - + are only active
	 at the start of a sentence *)
      if initial then begin
	let start = mkiter (Lexing.lexeme_start lexbuf) in
	stamp start start#forward_char Tags.Script.sentence;
	sentence true stamp mkiter lexbuf
      end else sentence false stamp mkiter lexbuf
    }
  | space+ {
       (* Parsing spaces is the only situation preserving initiality *)
       sentence initial stamp mkiter lexbuf
    }
  | _ {
      (* Any other characters *)
      sentence false stamp mkiter lexbuf
    }
  | eof { if initial then () else raise Unterminated }

{

  (** Parse sentences in string [slice], tagging relevant parts with
      function [stamp], in particular the sentence delimitors
      (either "." or "{" or "}" or the end of a comment).
      It will raise [Unterminated] when eof is encountered
      in the middle of a sentence.
  *)

  let delimit_sentences stamp mkiter slice =
    sentence true stamp mkiter (Lexing.from_string slice)

}
