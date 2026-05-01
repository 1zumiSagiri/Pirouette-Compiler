(** HTTP send/receive runtime for Pirouette choreographies.

    Provides the HTTP runtime communication layer for compiled Pirouette
    programs. It manages per-location message queues and an HTTP server that
    routes incoming messages into those queues. *)

val config : Config_parser.config option ref
(** Global configuration reference. Set this before calling any other function.
    Compiler-generated code assigns this from the loaded
    {!Config_parser.config}. *)

(** The following get functions are used by the generated ocaml code for sending
    messages *)

val get_body : string -> string -> Cohttp_eio.Body.t option
(** [get_body data sender] formats [data] and [sender] into an HTTP body string
    of the form ["data;sender"]. *)

val get_ip_address : location:string -> Uri.t
(** [get_ip_address ~location] returns the {!Uri.t} for [location] from the
    current config, or {!Uri.empty} if [location] is unknown. *)

val get_header : Http.Header.t
(** HTTP header with ["Connection: close"] set, ready for outgoing requests. *)

val marshal_data : 'a -> string
(** [marshal_data data] serializes [data] to a string via OCaml's Marshal
    module.

    Raises: any exception raised by [Marshal.to_string]. *)

val unmarshal_data : string -> ('a, string) result
(** [unmarshal_data s] deserializes [s] from a marshaled string. Returns
    [Ok value] on success, or [Error msg] if [s] is empty or unmarshaling fails.
*)

val init_http_server : string -> unit
(** [init_http_server location] initializes message queues and starts the
    HTTP server for [location].

    Requires: {!config} must be set before calling this function.

    Raises: [Failure] if [location] is not found in the configuration. *)

val receive_message : location:string -> string
(** [receive_message ~location] blocks until a message sent from [location] is
    available in the queue, then returns the raw (marshaled) message string. *)
