let c = Event.new_channel ()


let rec worker c = 
  let id =Thread.id (Thread.self ())
  in let v = Event.sync (Event.receive c)
  in print_string ((string_of_int v) ^ " " ^ (string_of_int id) ^ "\n");
  worker c

let rec thread_pool n =
  if n > 0 then begin ignore (Thread.create worker c); thread_pool (n - 1) end


let rec loop_forever () =
    let lst = [10; 20; 30; 40; 50; 60; 70; 80; 90; 100] in
    let elst = List.fold_right (fun x elst -> (Event.send c x) :: elst) lst [] in
  List.fold_right (fun e _ -> Event.sync e) elst (); loop_forever ()

let _ = 
  Format.printf "Here\n%!";
  thread_pool 5;
  print_string "Created 5 threads\n";
  loop_forever ()
