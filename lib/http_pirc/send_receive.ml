open Cohttp_eio

(** Global config reference, initialized by the compiler generated code. *)
let config = ref None

(* Message queues for each location *)
let loc_to_address = Hashtbl.create 10
let message_queues : (string, string Eio.Stream.t) Hashtbl.t = Hashtbl.create 10

(** Helper to get location config; [get_location_config location] is [Ok config]
    containing the HTTP address configuration for the [location] if [location]
    exists in the configuration, and [Error msg] if the configuration is not
    initialized or [location] is unknown.

    Requires: Configuration must be initialized before calling this function. *)
let get_location_config location =
  match !config with
  | None -> Error "Config not initialized. Call init() first"
  | Some cfg -> (
      match
        List.find_opt
          (fun loc -> loc.Config_parser.location = location)
          cfg.Config_parser.locations
      with
      | Some loc_config -> Ok loc_config
      | None -> Error ("Unknown location: " ^ location))

let get_body string_to_send participant_name_string =
  let list_to_send : string list = [] in
  let list_to_send = List.cons participant_name_string list_to_send in
  let list_to_send = List.cons string_to_send list_to_send in
  let final_string = String.concat ";" list_to_send in
  let body_to_send = Some (Body.of_string final_string) in
  body_to_send

let get_ip_address ~location =
  match get_location_config location with
  | Ok loc_config -> Uri.of_string loc_config.http_address
  | _ -> Uri.empty

let get_header =
  let new_header = Http.Header.init () in
  let header_to_send = Http.Header.add new_header "Connection" "close" in
  header_to_send

(* Function to marshal data *)
let marshal_data data =
  try
    let result = Marshal.to_string data [] in
    result
  with e -> raise e

(** [unmarshal_data data_str] is [Ok value] containing the unmarshaled OCaml
    value if [data_str] is a valid marshaled string, and [Error msg] if
    [data_str] is empty or unmarshaling fails.

    Requires: [data_str] must be a marshaled OCaml value. *)
let unmarshal_data data_str =
  try
    if String.length data_str = 0 then Error "Empty data string"
    else
      let result = Marshal.from_string data_str 0 in
      Ok result
  with e -> Error ("Unmarshal error: " ^ Printexc.to_string e)

(* DO NOT DELETE THIS IS A BACKUP *)
(* This is the handler for incoming http requests *)
let handler _socket _request body =
  let x : Cohttp_eio.Body.t = body in
  let sender_body = Eio.Buf_read.(parse_exn take_all) ~max_size:max_int x in
  let sep = ';' in
  let recv_list = String.split_on_char sep sender_body in
  let sender_location = List.nth_opt recv_list 0 in
  let sender_body = List.nth recv_list 1 in
  match sender_location with
  | None ->
      Cohttp_eio.Server.respond_string ~status:`Precondition_failed
        ~body:"Error message - Sender location not found" ()
  | Some unwrapped_sender_location -> (
      let indexed_queue =
        Hashtbl.find_opt message_queues unwrapped_sender_location
      in
      match indexed_queue with
      | Some result_queue ->
          Eio.Stream.add result_queue sender_body;
          Cohttp_eio.Server.respond_string ~status:`OK
            ~body:"Added to Htbl ; existing key" ()
      | None ->
          Cohttp_eio.Server.respond_string ~status:`Precondition_failed
            ~body:"This should not happen" ())

let setup_config_file () =
  (* First collect all original addresses *)
  match !config with
  | None -> ()
  | Some cfg ->
      List.iter
        (fun loc_cfg ->
          Hashtbl.add loc_to_address loc_cfg.Config_parser.location
            loc_cfg.Config_parser.http_address)
        cfg.Config_parser.locations;
      List.iter
        (fun loc_cfg ->
          Hashtbl.add message_queues loc_cfg.Config_parser.location
            (Eio.Stream.create 100))
        cfg.Config_parser.locations;
      let new_locations =
        List.map
          (fun loc_cfg ->
            {
              loc_cfg with
              Config_parser.http_address =
                Hashtbl.find loc_to_address loc_cfg.Config_parser.location;
            })
          cfg.Config_parser.locations
      in
      (* Update the config reference *)
      config := Some { Config_parser.locations = new_locations }

(* Initialize HTTP server for this location *)
let init_http_server current_location () =
  let () = setup_config_file () in
  match get_location_config current_location with
  | Error msg ->
      failwith
        ("location config" ^ current_location
       ^ "not found inside init_http_location" ^ msg)
  | Ok loc_config ->
      let uri = Uri.of_string loc_config.Config_parser.http_address in
      let uri_host =
        match Uri.host uri with
        | Some string_value -> string_value
        | None -> failwith "No value for uri host"
      in
      let unix_inet_addr = Unix.inet_addr_of_string uri_host in
      let address_to_run_server = Eio_unix.Net.Ipaddr.of_unix unix_inet_addr in
      let port_to_use : int =
        match Uri.port uri with Some p -> p | None -> 8080
      in
      (* The following statement sets up logs for debugging *)
      let () = Logs.set_reporter (Logs_fmt.reporter ())
      and () = Logs.Src.set_level Cohttp_eio.src None in
      let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex) in
      (* This runs the Eio event loop for the server *)
      let () =
        let port = ref port_to_use in
        Arg.parse
          [
            ("-p", Arg.Set_int port, " Listening port number(8080 by default)");
          ]
          ignore "An HTTP/1.1 server";
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        let socket =
          Eio.Net.listen env#net ~sw ~backlog:30000 ~reuse_port:true
            ~reuse_addr:true
            (`Tcp (address_to_run_server, !port))
        in
        let server = Cohttp_eio.Server.make ~callback:handler () in
        let dom_mgr = Eio.Stdenv.domain_mgr env in
        Cohttp_eio.Server.run socket server
          ?additional_domains:
            (Some (dom_mgr, Domain.recommended_domain_count ()))
          ?max_connections:(Some 30000) ~on_error:log_warning
      in
      ()

let rec receive_message ~location =
  let key_for_table = location in
  let stream_handle_option = Hashtbl.find_opt message_queues key_for_table in
  match stream_handle_option with
  | Some stream_associated_key -> (
      let value_from_stream_handle =
        Eio.Stream.take_nonblocking stream_associated_key
      in
      match value_from_stream_handle with
      | Some value_from_stream -> value_from_stream
      | None -> receive_message ~location)
  | None -> receive_message ~location
