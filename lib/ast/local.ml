type filename = string
type line = int

type metainfo = filename * line 

type value =
  [ `Int of int * metainfo
  | `String of string * metainfo
  | `Bool of bool * metainfo]

type loc_id = LocId of string * metainfo
type var_id = VarId of string * metainfo
type sync_label = LabelId of string * metainfo

type bin_op =
  | Plus of metainfo
  | Minus of metainfo
  | Times of metainfo
  | Div of metainfo
  | And of metainfo
  | Or of metainfo
  | Eq of metainfo
  | Neq of metainfo
  | Lt of metainfo
  | Leq of metainfo
  | Gt of metainfo
  | Geq of metainfo

type local_type =
  | TUnit of metainfo
  | TInt of metainfo
  | TString of metainfo
  | TBool of metainfo
  | TProd of local_type * local_type * metainfo
  | TSum of local_type * local_type * metainfo

type local_pattern =
  | Default of metainfo
  | Val of value * metainfo
  | Var of var_id * metainfo
  | Pair of local_pattern * local_pattern * metainfo
  | Left of local_pattern * metainfo
  | Right of local_pattern * metainfo

type local_expr =
  | Unit of metainfo
  | Val of value * metainfo
  | Var of var_id * metainfo
  | BinOp of local_expr * bin_op * local_expr * metainfo
  | Let of var_id * local_expr * local_expr * metainfo
  | Pair of local_expr * local_expr * metainfo
  | Fst of local_expr * metainfo
  | Snd of local_expr * metainfo
  | Left of local_expr * metainfo
  | Right of local_expr * metainfo
  | Match of local_expr * (local_pattern * local_expr) list * metainfo
