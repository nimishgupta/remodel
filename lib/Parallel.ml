let collector_ch    = Event.new_channel () (* int option channel     *)
let worker_ch       = Event.new_channel () (* Build.t option channel *)
let results_ch      = Event.new_channel () (* Build.t channel        *)
let comb_results_ch = Event.new_channel () (* Build.t list channel   *)

let dispatch (pre_process : 'a -> 'b) (arg : 'a) : unit =
  let open Event in
  let arg' = pre_process arg in
  sync (send worker_ch (Some arg'))


let list_parallel (pre_process  : 'a -> 'b)
                  (post_process : 'c -> unit)
                  (alst : 'a list) : unit =
  assert ([] <> alst);
  let len = List.length alst in
  Event.sync (Event.send collector_ch (Some len));
  List.iter (dispatch pre_process) alst;
  let rlst = Event.sync (Event.receive comb_results_ch) in
  assert ([] <> rlst);
  List.iter post_process rlst


let rec collector () : unit =
  match Event.sync (Event.receive collector_ch) with
    | None -> ()
    | Some n ->
        let rec results n =
           if n = 0 then []
           else (Event.sync (Event.receive results_ch)) :: results (n - 1)
        in Event.sync (Event.send comb_results_ch (results n));
        collector ()


let rec worker (compute : 'a -> 'b) : unit =
  match Event.sync (Event.receive worker_ch) with
    | None -> () 
    | Some arg ->
        try
          let res = compute arg in
          Event.sync (Event.send results_ch res);
          worker compute
        with Build.Build_error str -> Log.error str 1


let rec thread_pool (worker : 'a -> 'b) (arg : 'a) (n : int) : Thread.t list =
  if n = 0 then []
  else (Thread.create worker arg) :: thread_pool worker arg (n - 1)


type t = {
           collector_tid : Thread.t;
           worker_tids   : Thread.t list;
         }


let start (pre_process : Vertex.t -> Build.t) 
          (compute : Build.t -> Build.t)
          (post_process : Build.t -> unit)
          (size : int) : t * (Vertex.t list -> unit) =
  let coll_tid  = List.hd (thread_pool collector () 1) in
  let wrkr_tids = thread_pool worker compute size in
  let session = { 
                  collector_tid = coll_tid;
                  worker_tids = wrkr_tids;
                }
  in session, (list_parallel pre_process post_process)


let terminate (session : t) : unit =
  let open Event in
  List.iter (fun _ -> sync (send worker_ch None)) session.worker_tids;
  sync (send collector_ch None);
  List.iter Thread.join session.worker_tids;
  Thread.join session.collector_tid
