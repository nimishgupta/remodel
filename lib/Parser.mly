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

targets:
  | files { $1 }

production:
  | targets BUILT_BY files        
    { 
      (* Since no action corresponds, no tweaking required *)
      let deps   = to_deps $3 in
      let action = to_action None in
      List.fold_right (fun t plst -> (to_target t, deps, action) :: plst) $1 []
    }

  | targets BUILT_BY files ACTION
    { 
      (* tweat graph a little such that multiple targets built by same
       * action gets transformed into one of them built by those action
       * and others are obtained without executing any action from the
       * chosen one
       *)
      let open List in
      let deps = to_deps $3 in
      let action = to_action (Some $4) in
      let fst = hd $1 in
      let rst = tl $1 in
      let f t plst = ((to_target t), (to_deps [fst]), to_action None) :: plst in
      fold_right f rst [to_target fst, deps, action]
    }

productions:
  | production             { $1 }
  | production productions { List.append $1 $2 }

program:
  | productions EOF { to_rules $1 }

%%
