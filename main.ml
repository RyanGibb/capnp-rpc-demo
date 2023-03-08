
let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Eio_main.run @@ fun env ->
  let buf = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~initial_size:100 ~max_size:1_000_000 in
  while true do
    let line = Eio.Buf_read.line buf in
    let service = Echo.local in
    let reply = Echo.ping service line in
    Eio.Flow.copy_string (reply ^ "\n") (Eio.Stdenv.stdout env)
  done
