(** Configuration parsing for Pirouette choreographies.

    Handles parsing of YAML configuration files that map choreographic
    locations to concrete HTTP addresses. *)

type location_config = {
  location : string;
      (** Location identifier as it appears in the choreography. *)
  http_address : string;
      (** HTTP address where this location will listen and send. *)
}
(** Per-location HTTP configuration. *)

type config = {
  locations : location_config list;  (** All location configurations. *)
}
(** Complete configuration for all locations in a choreography. *)

val load_config : string -> config option Lwt.t
(** [load_config filename] reads and parses the YAML config at [filename].
    Returns [Some config] on success, or [None] if the file is missing or
    malformed.

    Requires: [filename] points to a valid YAML file. *)

val check_locations :
  config ->
  'a Ast_core.Choreo.M.stmt_block ->
  (location_config list, string) result
(** [check_locations] accepts a [config choreo_ast] value, and returns
    [Ok locations] containing only the locations from [config] that are used in
    [choreo_ast] if all locations in [choreo_ast] have corresponding entries in
    [config]. Returns [Error msg] listing the undefined locations otherwise.

    Requires: [choreo_ast] is a valid choreography AST. *)
