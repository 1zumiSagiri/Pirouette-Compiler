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

(*construct error message*)
let handle_err typ_err arg =
  let err_msg =
    match typ_err with
    | UnboundVariable -> "Unbound variable: " ^ arg
    | BopErr -> "operator and operands mismatch, expected: " ^ arg
    | LocalPattnErr -> "Error in local pattern, detail: " ^ arg
    | LocalPattnMismatch -> "Pattern and expression mismatchL: " ^ arg
    | LocalExpErr -> "Error in local expression, detail: " ^ arg
  in
  Error err_msg
(* failwith err_msg *)

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*raise exception if pair is not found*)
let context_lookup ctx var_name =
  match List.assoc_opt var_name ctx with
  | Some t -> Ok t
  | None -> handle_err UnboundVariable var_name


let typeof_bop bop e1 e2 =
  match bop with
  | Ast.Local.Plus | Ast.Local.Minus | Ast.Local.Times | Ast.Local.Div -> (
      match (e1, e2) with
      | Ast.Local.TInt, Ast.Local.TInt -> Ok Ast.Local.TInt
      | _ -> handle_err BopErr "Local.TInt -> Local.TInt -> Local.TInt")
  | Ast.Local.Eq | Ast.Local.Neq | Ast.Local.Lt | Ast.Local.Gt | Ast.Local.Geq
  | Ast.Local.Leq -> (
      match (e1, e2) with
      | Ast.Local.TInt, Ast.Local.TInt -> Ok Ast.Local.TBool
      | _ -> handle_err BopErr "Local.TInt -> Local.TInt -> Local.TBool")
  | Ast.Local.And | Ast.Local.Or -> (
      match (e1, e2) with
      | Ast.Local.TBool, Ast.Local.TBool -> Ok Ast.Local.TBool
      | _ -> handle_err BopErr "Local.TBool -> Local.TBool -> Local.TBool")


let rec check_local_pattn ctx p =
  match p with
  | Ast.Local.Default -> Ok Ast.Local.TUnit
  | Ast.Local.Val v -> (
      match v with
      | `Int _ -> Ok Ast.Local.TInt
      | `String _ -> Ok Ast.Local.TString
      | `Bool _ -> Ok Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.Pair (p1, p2) -> (
      let r1 = check_local_pattn ctx p1 in
      let r2 = check_local_pattn ctx p2 in
      match (r1, r2) with
      | Ok p1_typ, Ok p2_typ -> Ok (Ast.Local.TProd (p1_typ, p2_typ))
      | Error err1, Ok _ -> handle_err LocalPattnErr err1
      | Ok _, Error err2 -> handle_err LocalPattnErr err2
      | Error err1, Error err2  -> handle_err LocalPattnErr ("Error 1:\n" ^ err1 ^ "\nError 2:\n" ^ err2))
  (*TODO: not sure left and right*)
  | Ast.Local.Left p -> (
      match check_local_pattn ctx p with
      | Ok _ -> Ok Ast.Local.TUnit
      | Error err -> handle_err LocalPattnErr ("Expect: Ast.Local.Tunit\n" ^ err))
  | Ast.Local.Right p -> (
      match check_local_pattn ctx p with
      | Ok _ -> Ok Ast.Local.TUnit
      | Error err -> handle_err LocalPattnErr ("Expect: Ast.Local.Tunit" ^ err))

and check_local_case ctx match_typ p e =
    let p_typ = check_local_pattn ctx p in
    if p_typ = match_typ then check_local_exp ctx e
    else handle_err LocalPattnMismatch "Pattern and expression mismatch"

(*type checking of local expression*)
and check_local_exp ctx e =
  match e with
  | Ast.Local.Unit -> Ok Ast.Local.TUnit
  | Ast.Local.Val v -> (
      match v with
      | `Int _ -> Ok Ast.Local.TInt
      | `String _ -> Ok Ast.Local.TString
      | `Bool _ -> Ok Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.BinOp (e1, bop, e2) ->(
      let r1 = check_local_exp ctx e1 in
      let r2 = check_local_exp ctx e2 in
      match (r1, r2) with 
      | Ok e1_typ, Ok e2_typ -> typeof_bop bop e1_typ e2_typ
      | Error err1, Ok _ -> handle_err LocalExpErr err1
      | Ok _, Error err2 -> handle_err LocalExpErr err2
      | Error err1, Error err2  -> handle_err LocalExpErr ("Error 1:\n" ^ err1 ^ "\nError 2:\n" ^ err2))
  | Ast.Local.Let (Ast.Local.VarId var_name, e1, e2) ->(
      let r1 = check_local_exp ctx e1 in
      match r1 with
      | Ok e1_typ -> check_local_exp (add_binding ctx var_name e1_typ) e2
      | Error err -> handle_err LocalExpErr err)
  | Ast.Local.Pair (e1, e2) ->(
      let r1 = check_local_exp ctx e1 in
      let r2 = check_local_exp ctx e2 in
      match (r1,r2) with
      | Ok e1_typ, Ok e2_typ -> Ok (Ast.Local.TProd (e1_typ, e2_typ))
      | Error err1, Ok _ -> handle_err LocalExpErr err1
      | Ok _, Error err2 -> handle_err LocalExpErr err2
      | Error err1, Error err2  -> handle_err LocalExpErr ("Error 1:\n" ^ err1 ^ "\nError 2:\n" ^ err2))
  | Ast.Local.Fst e -> (
      match check_local_exp ctx e with
      | Ok Ast.Local.TProd (t1, _) -> Ok t1
      | _ -> handle_err LocalExpErr "Expect Local Pair")
  | Ast.Local.Snd e -> (
      match check_local_exp ctx e with
      | Ok Ast.Local.TProd (_, t2) -> Ok t2
      | _ -> handle_err LocalExpErr "Expect Local Pair")
  | Ast.Local.Left e -> (
      match check_local_exp ctx e with
      | Ok t -> Ok (Ast.Local.TSum (t, Ast.Local.TUnit))
      | _ -> handle_err BopErr "Local.TInt -> Local.TUnit")
  | Ast.Local.Right e -> (
      match check_local_exp ctx e with
      | Ok t -> Ok (Ast.Local.TSum (Ast.Local.TUnit, t))
      | _ -> handle_err BopErr "Local.TInt -> Local.TUnit")
  | Ast.Local.Match (e, cases) ->
      let e_typ = check_local_exp ctx e in
      match cases with
        | [] -> handle_err LocalExpErr "Empty case"
        | (p1, e1) :: tl ->
            let r1 = check_local_case ctx e_typ p1 e1 in
            if r1 = Ok Ast.Local.TUnit then
              check_local_exp ctx (Ast.Local.Match (e, tl))
            else r1
