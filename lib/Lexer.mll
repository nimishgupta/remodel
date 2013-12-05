{
  open Parser
}

let blank   = [ ' ' '\t' ]
let newline = '\n' | '\r' | "\r\n"
let file    = [^ '\000' ':' ',' '<' '\n' ] +

rule token = parse
  | blank+      { token lexbuf                         }
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | eof         { EOF                                  }
  | "<-"        { BUILT_BY                             }
  | ","         { COMMA                                }
  | ":"         { action lexbuf                        }
  | file as f   { FILE (String.trim f)                 }

and action = parse
  | [^ '\n']+ as a { ACTION (String.trim a); }
  | '\n'           { Lexing.new_line lexbuf; token lexbuf }
