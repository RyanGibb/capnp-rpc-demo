
let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ());
  Eio_main.run @@ fun env ->
  let connection = Pipe.Connection.local in
  Capnp_rpc_lwt.Capability.pp Format.std_formatter connection; Format.print_newline ();
  let stream = Pipe.Connection.create connection in
  Capnp_rpc_lwt.Capability.pp Format.std_formatter stream; Format.print_newline ();
  Eio.Fiber.both
    (fun () ->
      let rec write_from_stdin () =
        let buf = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~initial_size:100 ~max_size:1_000_000 in
        let line = Eio.Buf_read.line buf in
        match Pipe.Stream.write stream line with
        | Ok () -> write_from_stdin ()
        | Error (`Capnp e) -> Capnp_rpc.Error.pp Format.std_formatter e
      in write_from_stdin ()
    )
    (fun () ->
      let rec read_to_stdout () =
        match Pipe.Stream.read stream with
        | Ok data ->
          Eio.Flow.copy_string (data ^ "\n") (Eio.Stdenv.stdout env); Format.print_newline ();
          read_to_stdout ()
        | Error (`Capnp e) -> Capnp_rpc.Error.pp Format.std_formatter e; Format.print_newline ();
      in read_to_stdout ()
    );;
