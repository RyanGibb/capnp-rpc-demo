module Api = Pipe_api.MakeRPC(Capnp_rpc_lwt)

open Capnp_rpc_lwt

let local stdout clock =
  let module Pipe = Api.Service.Pipe in
  Pipe.local @@ object
    inherit Pipe.service

    method read_impl _params release_param_caps =
      let open Pipe.Read in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Eio.Time.sleep clock 1.0;
      Results.data_set results "-> ...";
      Service.return response

    method write_impl params release_param_caps =
      let open Pipe.Write in
      let data = Params.data_get params in
      Eio.Flow.copy_string ("<-" ^ data ^ "\n") stdout;
      release_param_caps ();
      let response, _results = Service.Response.create Results.init_pointer in
      Service.return response

  end

module Pipe = Api.Client.Pipe

let read t =
  let open Pipe.Read in
  let request, _params = Capability.Request.create Params.init_pointer in
  Capability.call_for_value_exn t method_id request |> Results.data_get

let write t data =
  let open Pipe.Write in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.data_set params data;
  ignore (Capability.call_for_value_exn t method_id request)