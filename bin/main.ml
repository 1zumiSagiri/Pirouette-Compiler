open Ast.Interface
open Parsing.Interface

(*err msg of type checking*)
let unbound_var_err var = "Unbound variable: " ^ var
let bop_err = "operator and operands mismatch"
let if_cond_err = "cond of if is not boolean"

(*context: list of pair of variable name and its binding*)
type lcl_context = (string * Ast.Local.local_type) list
type choreo_context = (string * Ast.Choreo.choreo_type) list

let add_binding ctx var_name var_type = (var_name, var_type) :: ctx 

let context_lookup ctx var_name =
  try List.assoc var_name ctx
  with Not_found -> raise (failwith (unbound_var_err var_name))

let typeof_bop bop e1 e2 =
  match bop with
  | Ast.Local.Plus | Ast.Local.Minus | Ast.Local.Times | Ast.Local.Div -> (
    match e1, e2 with
    | (Ast.Local.TInt, Ast.Local.TInt) -> Ast.Local.TInt
    | _ -> raise (failwith bop_err))
  | Ast.Local.Eq | Ast.Local.Neq | Ast.Local.Lt |  Ast.Local.Gt | Ast.Local.Geq | Ast.Local.Leq -> (
    match e1, e2 with
    | (Ast.Local.TInt, Ast.Local.TInt) -> Ast.Local.TBool
    | _ -> raise (failwith bop_err))
  | Ast.Local.And | Ast.Local.Or -> (
    match e1, e2 with
    | (Ast.Local.TBool, Ast.Local.TBool) -> Ast.Local.TBool
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
