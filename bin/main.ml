open Ast.Interface
open Parsing.Interface

(*err msg of type checking*)
let unbound_var_err var = "Unbound variable: " ^ var
let bop_err = "operator and operands mismatch"
let if_cond_err = "cond of if is not boolean"

(*context: list of pair of variable name and its binding*)
(*Vincent: need to fix definition of typ, should add pattern and expr also*)
type typ = TLocal of Ast.Local.local_type | TChoreo of Ast.Choreo.choreo_type
type context = (string * typ) list

let add_binding ctx var_name var_type = (var_name, var_type) :: ctx 

let context_lookup ctx var_name =
  try List.assoc var_name ctx
  with Not_found -> raise (failwith (unbound_var_err var_name))

(*NOT finished yet*)
let rec typeof ctx = function
  | Ast.Local.TUnit -> TLocal Ast.Local.TUnit
  | Ast.Local.TInt -> TLocal Ast.Local.TInt
  | Ast.Local.TBool -> TLocal Ast.Local.TBool
  | Ast.Local.TString -> TLocal Ast.Local.TString
  | Ast.Local.TProd (t1, t2) -> TLocal (Ast.Local.TProd (typeof ctx t1, typeof ctx t2))
  | Ast.Local.TSum (t1, t2) -> TLocal (Ast.Local.TSum (typeof ctx t1, typeof ctx t2))
  | _ -> raise (failwith "type not implemented")

let typeof_bop ctx bop e1 e2 =
  let (t1 , t2) = typeof ctx e1, typeof ctx e2 in
  match bop with
  | Ast.Local.Plus | Ast.Local.Minus | Ast.Local.Times | Ast.Local.Div -> (
    match t1, t2 with
    | (TLocal Ast.Local.TInt, TLocal Ast.Local.TInt) -> TLocal Ast.Local.TInt
    | _ -> raise (failwith bop_err))
  | Ast.Local.Eq | Ast.Local.Neq | Ast.Local.Lt |  Ast.Local.Gt | Ast.Local.Geq | Ast.Local.Leq -> (
    match t1, t2 with
    | (TLocal Ast.Local.TInt, TLocal Ast.Local.TInt) -> TLocal Ast.Local.TBool
    | _ -> raise (failwith bop_err))
  | Ast.Local.And | Ast.Local.Or -> (
    match t1, t2 with
    | (TLocal Ast.Local.TBool, TLocal Ast.Local.TBool) -> TLocal Ast.Local.TBool
    | _ -> raise (failwith bop_err))


let () =
  if Array.length Sys.argv < 2 then (
    print_endline "USAGE: pirc <file>";
    exit 1)
  else
    let filename = Sys.argv.(1) in
    let file_ic = open_in filename in
    let lexbuf = Lexing.from_channel file_ic in
    let program = parse_program lexbuf in
    print_endline (dump_choreo_ast program);
    close_in file_ic
