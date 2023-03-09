
let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let run_client ~stdin ~stdout service =
  Eio.Fiber.both
    (fun () ->
      let rec write_from_stdin () =
        let buf = Eio.Buf_read.of_flow stdin ~initial_size:100 ~max_size:1_000_000 in
        let line = Eio.Buf_read.line buf in
        match Pipe.write service line with
        | Ok () -> write_from_stdin ()
        | Error (`Capnp e) ->
          Capnp_rpc.Error.pp Format.std_formatter e;
          Format.print_flush ()
      in write_from_stdin ()
    )
    (fun () ->
      let rec read_to_stdout () =
        match Pipe.read service with
        | Ok data ->
          Eio.Flow.copy_string (data ^ "\n") stdout;
          read_to_stdout ()
        | Error (`Capnp e) ->
          Capnp_rpc.Error.pp Format.std_formatter e;
          Format.print_flush ()
      in read_to_stdout ()
    );;

let connect uri =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  Capnp_rpc_unix.with_cap_exn sr (run_client ~stdin:(Eio.Stdenv.stdin env) ~stdout:(Eio.Stdenv.stdout env))

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd =
  let doc = "run the client" in
  let info = Cmd.info "connect" ~doc in
  Cmd.v info Term.(const connect $ connect_addr)

let () =
  exit (Cmd.eval connect_cmd)
