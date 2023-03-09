
let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Eio_main.run @@ fun env ->
  let service = Pipe.local (Eio.Stdenv.stdout env) (Eio.Stdenv.clock env) in
  Eio.Fiber.both
    (fun () ->
      let rec write_from_stdin () =
        let buf = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~initial_size:100 ~max_size:1_000_000 in
        let line = Eio.Buf_read.line buf in
        match Pipe.write service line with
        | Ok () -> write_from_stdin ()
        | Error (`Capnp e) -> Capnp_rpc.Error.pp Format.std_formatter e
      in write_from_stdin ()
    )
    (fun () ->
      let rec read_to_stdout () =
        match Pipe.read service with
        | Ok data ->
          Eio.Flow.copy_string (data ^ "\n") (Eio.Stdenv.stdout env);
          read_to_stdout ()
        | Error (`Capnp e) -> Capnp_rpc.Error.pp Format.std_formatter e
      in read_to_stdout ()
    );;
