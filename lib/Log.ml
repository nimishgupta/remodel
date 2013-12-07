let error (str : string) (code : int) : unit  = 
  print_endline ("remodel: " ^ str); ignore (exit code)

let warning (str: string) : unit =
  print_endline ("remodel: Warning, " ^ str)

let info (str : string) : unit = 
  print_endline ("remodel: " ^ str)
