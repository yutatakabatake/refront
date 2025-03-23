let parse () = 
  let lexbuf = Lexing.from_channel stdin in 
  try
    let rlt = Parser.program Lexer.token lexbuf in 
    print_string (Ast_to_sexpr.program_to_sexpr rlt)
  with exn ->
    print_string (Lexing.lexeme lexbuf);
    raise exn

let _ = parse ()