
let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Eio_main.run @@ fun env ->
  let service = Pipe.local (Eio.Stdenv.stdout env) (Eio.Stdenv.clock env) in
  Eio.Fiber.both
    (fun () ->
      while true do
        let buf = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~initial_size:100 ~max_size:1_000_000 in
        let line = Eio.Buf_read.line buf in
        Pipe.write service line;
      done  
      )
    (fun () ->
      while true do
        let reply = Pipe.read service in
        Eio.Flow.copy_string (reply ^ "\n") (Eio.Stdenv.stdout env)
      done  
      );;
