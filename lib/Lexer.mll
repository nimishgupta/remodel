{
  open Parser
}

let action = '"' _+ '"' '\n' 
let file   = [^ '\000']+

rule token = parse
  | eof       { EOF       }
  | action    { ACTION    }
  | "<-"      { BUILT_BY  }
  | ","       { COMMA     }
  | "DEFAULT" { DEFAULT   }
  | ":"       { EXECUTING }
  | file      { FILE      }
