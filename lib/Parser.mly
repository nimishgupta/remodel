%{
  open Rules
%}

%token <string> ACTION
%token BUILT_BY
%token COMMA
%token EOF
%token <string> FILE

%start program

%type <Rules.t> program

%%

/* TODO : pass position for displaying error messages */
files:
  | FILE              { [$1]     } 
  | FILE COMMA files  { $1 :: $3 }

target:
  | FILE { $1 }

/* TODO : Make use of "targets" production */
targets:
  | files { $1 }

/* TODO : Allow multiple targets (Consult make) */
production:
  | target BUILT_BY files        { to_target $1, to_deps $3, to_action None    }
  | target BUILT_BY files ACTION { to_target $1, to_deps $3, to_action Some $4 }

productions:
  | production             { [$1] }
  | production productions { $1 :: $2 }

program:
  | productions EOF { to_rules $1 }

%%
