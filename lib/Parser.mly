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

%type <DPDS.dependencies> program

%%

files:
  | FILE              { [$1] } 
  | FILE COMMA files  { $1 :: $3 }

targets:
  | files { $1 }

productions:
  | DEFAULT BUILT_BY targets                  { DPDS.default, $3, None     }
  | targets BUILT_BY files                    { DPDS.Files $1, $3, None    }
  | targets BUILT_BY files EXECUTING ACTION   { DPDS.Files $1, $3, Some $5 }

program:
  | productions EOF { $1 }
%%
