
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
        | Error (`Capnp e) -> Capnp_rpc.Error.pp Format.std_formatter e
      in write_from_stdin ()
    )
    (fun () ->
      let rec read_to_stdout () =
        match Pipe.read service with
        | Ok data ->
          Eio.Flow.copy_string (data ^ "\n") stdout;
          read_to_stdout ()
        | Error (`Capnp e) -> Capnp_rpc.Error.pp Format.std_formatter e
      in read_to_stdout ()
    );;

let secret_key = `File "secret-key.pem"
let listen_address = `TCP ("127.0.0.1", 7000)

let start_server ~sw ~stdout ~clock net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore = Capnp_rpc_net.Restorer.single service_id (Pipe.local ~stdout ~clock) in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let uri = start_server ~sw ~stdout:(Eio.Stdenv.stdout env) ~clock:(Eio.Stdenv.clock env) (Eio.Stdenv.net env) in
  Eio.traceln "Connecting to echo service at: %a" Uri.pp_hum uri;
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  Capnp_rpc_lwt.Sturdy_ref.with_cap_exn sr (run_client ~stdin:(Eio.Stdenv.stdin env) ~stdout:(Eio.Stdenv.stdout env));
  raise Exit
