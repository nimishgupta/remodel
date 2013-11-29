%{
  module DPDS = Dependencies
%}

%token <string> ACTION
%token BUILT_BY
%token COMMA
%token DEFAULT
%token EXECUTING
%token EOF
%token <string> FILE

%start program

%type <Dependencies.dependencies> program

%%

/* TODO : pass position for displaying error messages */
files:
  | FILE              { [$1] } 
  | FILE COMMA files  { $1 :: $3 }

targets:
  | files { $1 }

/* TODO : Should not allow multiple default productions */
production:
  | DEFAULT BUILT_BY targets                  { Dependencies.Default,  $3, None    }
  | targets BUILT_BY files                    { Dependencies.Files $1, $3, None    }
  | targets BUILT_BY files EXECUTING ACTION   { Dependencies.Files $1, $3, Some $5 }

productions:
  | production             { [$1]     }
  | production productions { $1 :: $2 }

program:
  | productions EOF { $1 }

%%
