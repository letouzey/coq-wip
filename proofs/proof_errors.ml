exception Exception of exn
exception Timeout
exception TacticFailure of exn

let _ = Err.register_handler begin function
  | Timeout -> Err.errorlabstrm "Some timeout function" (Pp.str"Timeout!")
  | Exception e -> Err.print e
  | TacticFailure e -> Err.print e
  | _ -> Pervasives.raise Err.Unhandled
end


