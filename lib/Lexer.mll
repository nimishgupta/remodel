{
  open Parser
}

let action = '"' _+ '"' '\n' 
let file   = [^ '\000']+

rule token = parse
  | eof         { EOF       }
  | action as a { ACTION a  }
  | "<-"        { BUILT_BY  }
  | ","         { COMMA     }
  | "DEFAULT"   { DEFAULT   }
  | ":"         { EXECUTING }
  | file as f   { FILE f    }
