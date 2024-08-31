open Ast.Interface
open Parsing.Interface

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

(*************************************************)

(*err msg of type checking*)
type typ_err = UnboundVariable | BopErr | LocalPattnErr
              | LocalPattnMismatch | LocalExpErr

(*context: list of pair of variable name and its binding*)
type local_context = (string * Ast.Local.local_type) list
type choreo_context = (string * Ast.Choreo.choreo_type) list

(*construct and raise exception with error message*)
let handle_err typ_err arg =
  let err_msg =
    match typ_err with
    | UnboundVariable -> "Unbound variable: " ^ arg
    | BopErr -> "operator and operands mismatch, expected: " ^ arg
    | LocalPattnErr -> "Error in local pattern, detail: " ^ arg
    | LocalPattnMismatch -> "Pattern and expression mismatchL: " ^ arg
    | LocalExpErr -> "Error in local expression, detail: " ^ arg
  in failwith err_msg

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*raise exception if pair is not found*)
let context_lookup ctx var_name =
  match List.assoc_opt var_name ctx with
  | Some t -> t
  | None -> handle_err UnboundVariable var_name

let typeof_bop bop e1 e2 =
  match bop with
  | Ast.Local.Plus | Ast.Local.Minus | Ast.Local.Times | Ast.Local.Div -> (
      match (e1, e2) with
      | Ast.Local.TInt, Ast.Local.TInt -> Ast.Local.TInt
      | _ -> handle_err BopErr "Local.TInt -> Local.TInt -> Local.TInt")
  | Ast.Local.Eq | Ast.Local.Neq | Ast.Local.Lt | Ast.Local.Gt | Ast.Local.Geq
  | Ast.Local.Leq -> (
      match (e1, e2) with
      | Ast.Local.TInt, Ast.Local.TInt -> Ast.Local.TBool
      | _ -> handle_err BopErr "Local.TInt -> Local.TInt -> Local.TBool")
  | Ast.Local.And | Ast.Local.Or -> (
      match (e1, e2) with
      | Ast.Local.TBool, Ast.Local.TBool -> Ast.Local.TBool
      | _ -> handle_err BopErr "Local.TBool -> Local.TBool -> Local.TBool")


let rec check_local_pattn ctx p =
  match p with
  | Ast.Local.Default -> Ast.Local.TUnit
  | Ast.Local.Val v -> (
      match v with
      | `Int _ -> Ast.Local.TInt
      | `String _ -> Ast.Local.TString
      | `Bool _ -> Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.Pair (p1, p2) -> (
      Ast.Local.TProd (check_local_pattn ctx p1, check_local_pattn ctx p2))
  (*TODO: not sure left and right*)
  | Ast.Local.Left p -> (
      match check_local_pattn ctx p with
      | _ -> Ast.Local.TUnit)
  | Ast.Local.Right p -> (
      match check_local_pattn ctx p with
      | _ -> Ast.Local.TUnit)

and check_local_case ctx match_typ p e =
    let p_typ = check_local_pattn ctx p in
    if p_typ = match_typ then check_local_exp ctx e
    else handle_err LocalPattnMismatch "Pattern and expression mismatch"

(*type checking of local expression*)
and check_local_exp ctx e =
  match e with
  | Ast.Local.Unit -> Ast.Local.TUnit
  | Ast.Local.Val v -> (
      match v with
      | `Int _ -> Ast.Local.TInt
      | `String _ -> Ast.Local.TString
      | `Bool _ -> Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.BinOp (e1, bop, e2) ->(
      let (e1_typ, e2_typ) = (check_local_exp ctx e1, check_local_exp ctx e2) in
       typeof_bop bop e1_typ e2_typ)
  | Ast.Local.Let (Ast.Local.VarId var_name, e1, e2) ->(
      let e1_typ = check_local_exp ctx e1 in
       check_local_exp (add_binding ctx var_name e1_typ) e2)
  | Ast.Local.Pair (e1, e2) ->(
      let (e1_typ, e2_typ) = (check_local_exp ctx e1, check_local_exp ctx e2) in
      Ast.Local.TProd (e1_typ, e2_typ))
  | Ast.Local.Fst e -> (
      match check_local_exp ctx e with
      | Ast.Local.TProd (t1, _) -> t1
      | _ -> handle_err LocalExpErr "Expect Local Pair")
  | Ast.Local.Snd e -> (
      match check_local_exp ctx e with
      | Ast.Local.TProd (_, t2) -> t2
      | _ -> handle_err LocalExpErr "Expect Local Pair")
  | Ast.Local.Left e -> (
      Ast.Local.TSum (check_local_exp ctx e, Ast.Local.TUnit))
  | Ast.Local.Right e -> (
      Ast.Local.TSum (Ast.Local.TUnit, check_local_exp ctx e))
  | Ast.Local.Match (e, cases) ->
      let e_typ = check_local_exp ctx e in
        let _ = List.map (fun (p, e) -> check_local_case ctx e_typ p e) cases in
        e_typ

