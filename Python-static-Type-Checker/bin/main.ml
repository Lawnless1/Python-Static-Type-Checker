let () = print_endline "Inference Started ..."


exception NotImplemented of string
exception FunctionCallError of string*PyreAst.Concrete.Location.t
exception TypeError of string*PyreAst.Concrete.Location.t
(* exception ParseError of string *)

let red = "\x1b[31m"
let reset_color = "\x1b[0m"
let green = "\x1b[32m"



let debug = true

let loc_to_str (loc: PyreAst.Concrete.Location.t) : string =
  if loc.start.line = loc.stop.line then
    Printf.sprintf "%d | " loc.start.line
  else
    Printf.sprintf "%d-%d | " loc.start.line loc.stop.line

let print_dbg (loc: PyreAst.Concrete.Location.t) (msg: string) = 
  if debug then
    Printf.printf "%s%sDEBUG%s: %s\n" (loc_to_str loc) green reset_color msg
  else
    ()


module PSTC = struct
  type typ = 
    | None
    | Bool
    | Ellipsis
    | Int
    | Float
    | Complex
    | String
    | Bytes
    | List of typ
    | Tuple of typ
    | Dict of (typ * typ)
    | Set of typ
    | Frozenset of typ
    | Union of typ list
    | Function of typ list * typ
    | Lambda of typ list * typ
    | Any
    | Class of string
    | Unset
  
end


let rec typ_to_str (t: PSTC.typ) : string =
  let open PSTC in
    match t with
    | None -> "None"
    | Bool -> "bool"
    | Ellipsis -> "..."
    | Int -> "int"
    | Float -> "float"
    | Complex -> "complex"
    | String -> "str"
    | Bytes -> "bytes"
    | List t1 -> "list(" ^ typ_to_str t1 ^ ")"
    | Tuple ts ->
        "tuple(" ^ (typ_to_str ts) ^ ")"
    | Dict (k, v) ->
        "dict(" ^ typ_to_str k ^ ", " ^ typ_to_str v ^ ")"
    | Set t1 -> "set(" ^ typ_to_str t1 ^ ")"
    | Frozenset t1 -> "frozenset(" ^ typ_to_str t1 ^ ")"
    | Union ts ->
        "Union(" ^ String.concat ", " (List.map typ_to_str ts) ^ ")"
    | Function (args, ret) ->
        "Function(" ^
        String.concat ", " (List.map typ_to_str args) ^
        " -> " ^ typ_to_str ret ^ ")"
    | Lambda (args, ret) ->
        "Lambda(" ^
        String.concat ", " (List.map typ_to_str args) ^
        " -> " ^ typ_to_str ret ^ ")"
    | Any -> "Any"
    | Class name -> "class \"" ^ name ^ "\""
    | Unset -> "Unset"
  
let get_location (statement: PyreAst.Concrete.Statement.t) : PyreAst.Concrete.Location.t =
  match statement with
  | FunctionDef { location; _ }
  | AsyncFunctionDef { location; _ }
  | ClassDef { location; _ }
  | Return { location; _ }
  | Delete { location; _ }
  | Assign { location; _ }
  | TypeAlias { location; _ }
  | AugAssign { location; _ }
  | AnnAssign { location; _ }
  | For { location; _ }
  | AsyncFor { location; _ }
  | While { location; _ }
  | If { location; _ }
  | With { location; _ }
  | AsyncWith { location; _ }
  | Match { location; _ }
  | Raise { location; _ }
  | Try { location; _ }
  | TryStar { location; _ }
  | Assert { location; _ }
  | Import { location; _ }
  | ImportFrom { location; _ }
  | Global { location; _ }
  | Nonlocal { location; _ }
  | Expr { location; _ }
  | Pass { location; _ }
  | Break { location; _ }
  | Continue { location; _ } -> location

let rec equ_typ (t1: PSTC.typ) (t2: PSTC.typ) : bool =
  match t1, t2 with
  | PSTC.None, PSTC.None -> true
  | PSTC.Bool, PSTC.Bool -> true
  | PSTC.Ellipsis, PSTC.Ellipsis -> true
  | PSTC.Int, PSTC.Int -> true
  | PSTC.Float, PSTC.Float -> true
  | PSTC.Complex, PSTC.Complex -> true
  | PSTC.String, PSTC.String -> true
  | PSTC.Bytes, PSTC.Bytes -> true
  | PSTC.List t1', PSTC.List t2' -> equ_typ t1' t2'
  | PSTC.Tuple t1', PSTC.Tuple t2' -> equ_typ t1' t2'
  | PSTC.Dict (k1, v1), PSTC.Dict (k2, v2) -> equ_typ k1 k2 && equ_typ v1 v2
  | PSTC.Set t1', PSTC.Set t2' -> equ_typ t1' t2'
  | PSTC.Frozenset t1', PSTC.Frozenset t2' -> equ_typ t1' t2'
  | PSTC.Union ts1, PSTC.Union ts2 ->
      List.length ts1 = List.length ts2 &&
      List.for_all2 equ_typ ts1 ts2
  | PSTC.Function (args1, ret1), PSTC.Function (args2, ret2) ->
      List.length args1 = List.length args2 &&
      List.for_all2 equ_typ args1 args2 && 
      equ_typ ret1 ret2
  | PSTC.Lambda (args1, ret1), PSTC.Lambda (args2, ret2) ->
      List.length args1 = List.length args2 &&
      List.for_all2 equ_typ args1 args2 && 
      equ_typ ret1 ret2
  |PSTC.Union ts1, other ->
      List.exists (fun t -> equ_typ t other) ts1
  | other, PSTC.Union ts2 ->
      List.exists (fun t -> equ_typ other t) ts2
  | PSTC.Class name1, PSTC.Class name2 -> name1 = name2
  | PSTC.Any, _ -> true
  | _, PSTC.Any -> true
  | _ -> false

let not_equ_typ (t1: PSTC.typ) (t2: PSTC.typ) : bool =
  not (equ_typ t1 t2)

  let (==) = equ_typ
  
  let (!=) = not_equ_typ



let id_to_typ (id: PyreAst.Concrete.Identifier.t) : PSTC.typ =
  let id_str = PyreAst.Concrete.Identifier.to_string id in
  let open PSTC in
    match id_str with
    | "None" -> None
    | "bool" -> Bool
    | "ellipsis" -> Ellipsis
    | "int" -> Int
    | "float" -> Float
    | "complex" -> Complex
    | "str" -> String
    | "bytes" -> Bytes
    | "list" -> List Any
    | "tuple" -> Tuple Any
    | "dict" -> Dict (Any, Any)
    | "set" -> Set Any
    | "frozenset" -> Frozenset Any
    | "union" -> Union [Any]
    | "function" -> Function ([Any], Any)
    | "lambda" -> Lambda ([Any], Any)
    | _ -> raise (NotImplemented ("Type for identifier '" ^ id_str ^ "' is not implemented."))
  

    (* adding to dictionary*)
    let assign (assigned: PSTC.typ): PSTC.typ =
      let open PSTC in
      match assigned with
      | Function (_, ret) ->
          ret
      | other ->
        other 


let str_to_statements (content: string): (PyreAst.Concrete.Statement.t list) =
  let open PyreAst.Parser in
  with_context (fun context ->
  	match Concrete.parse_module ~context content with
  	| Result.Error { Error.message; line; column; _ } ->
  	  let message = 
  	    Format.sprintf "Parsing error at line %d, column %d: %s"
  	    line column message
  	  in
  	  failwith message
  	| Result.Ok ast -> ast.body
  )


let rec infer_type (expr: PyreAst.Concrete.Expression.t) (ctx_map : (string, PSTC.typ) Hashtbl.t) : PSTC.typ =
  let open PyreAst.Concrete.Expression in
  match expr with
  | Name {id; ctx; _} ->
    let id_str = PyreAst.Concrete.Identifier.to_string id in
      (match ctx with
        | Load -> 
          if Hashtbl.mem ctx_map id_str then
            Hashtbl.find ctx_map id_str
          else
            id_to_typ id
        | Store -> 
          if Hashtbl.mem ctx_map id_str then
            Hashtbl.find ctx_map id_str
          else
            PSTC.Unset
        | Del -> raise (NotImplemented "Delete context is not implemented")
      )
  | Constant {value; _} ->
      (match value with
        |None -> PSTC.None
        | False -> PSTC.Bool
        | True -> PSTC.Bool
        | Ellipsis -> PSTC.Ellipsis
        | Integer _ -> PSTC.Int
        | BigInteger _ -> PSTC.Int
        | Float _ -> PSTC.Float
        | Complex _ -> PSTC.Complex
        | String _ -> PSTC.String
        | ByteString _ -> PSTC.Bytes
      )
  | BinOp {left; right; op; _} ->
      let left_type = infer_type left ctx_map in
      let right_type = infer_type right ctx_map in
      (match op with 
      
        | BitOr -> (match left_type, right_type with
          | PSTC.Union ts1, PSTC.Union ts2 ->
              PSTC.Union (ts1 @ ts2)
          | PSTC.Union ts1, _ ->
              PSTC.Union (right_type :: ts1)
          | _, PSTC.Union ts2 ->
              PSTC.Union (left_type :: ts2)
          | _, _ ->
              if left_type == right_type then
                left_type
              else
                PSTC.Union [left_type; right_type]
          ) 
        | _ -> raise (NotImplemented "Binary operation type inference is not implemented"))
  | Call {location; func; args; _} ->
    let got_func_type = 
      let arg_types = List.map (fun arg -> infer_type arg ctx_map) args in
      PSTC.Function (arg_types, PSTC.Any) (* Return type is not known *) in
    let expected_func_type = infer_type func ctx_map in
    if got_func_type == expected_func_type then
      expected_func_type
    else
      raise (FunctionCallError (
        (Printf.sprintf "Function call type mismatch: expected %s, got %s"
          (typ_to_str expected_func_type) (typ_to_str got_func_type)), location))

  | _ -> raise (NotImplemented ("Type inference for this expression is not yet implemented"))

let rec process_statements (statements: (PyreAst.Concrete.Statement.t list)) (ctx_map : (string, PSTC.typ) Hashtbl.t): unit =
  let open PyreAst.Concrete.Statement in
  List.iter (function statement -> try (
    match statement with
    | FunctionDef {location; name; body; args; returns ; _ } ->
        let return_type = (match returns with
          | Some ret -> infer_type ret ctx_map
          | None -> PSTC.Unset) in
        let args_ls = args.args in

        (* Infer types for function arguments *)
        let arg_types = 
          List.map (fun arg ->
            match arg with
            | PyreAst.Concrete.Argument.{annotation; _} ->
                match annotation with
                | Some ann -> infer_type ann ctx_map
                | None -> PSTC.Unset
          ) args_ls in
        (* Print function information *)
        print_dbg location (Printf.sprintf "Found function %s: %s -> %s" (PyreAst.Concrete.Identifier.to_string name)
          (String.concat ", " (List.map typ_to_str arg_types))
          (typ_to_str return_type));
        (* Add function to context map *)
        Hashtbl.add ctx_map (PyreAst.Concrete.Identifier.to_string name) 
          (PSTC.Function (arg_types, return_type));
        (* Process function body *)
        process_statements body ctx_map
    | ClassDef {location; name; body; _ } ->
        print_dbg location (Printf.sprintf "Found class: %s" (PyreAst.Concrete.Identifier.to_string name));
        process_statements body ctx_map
    | Assign {location; targets; value; _} ->
      let targets_types = List.map 
        (fun target -> match target with
        | PyreAst.Concrete.Expression.Name {id; _}->
            let id_str = PyreAst.Concrete.Identifier.to_string id in
            if Hashtbl.mem ctx_map id_str then
              Hashtbl.find ctx_map id_str
            else
              (*Target not found in context map, using Unset type*)
              PSTC.Unset
        | _ -> 
          (* target is not a name*)
          raise (NotImplemented "Annotation assignment is not fully implemented"))
         targets in
      let value_type = assign (infer_type value ctx_map) in
      if List.map (fun t -> if t == value_type then true else false) targets_types |> List.for_all (fun x -> x) then
        (*All targets have the same type as value *)
        print_dbg location (Printf.sprintf "Assign: %s <- %s" 
          (String.concat ", " (List.map (fun target -> typ_to_str (infer_type target ctx_map)) targets))
          (typ_to_str (infer_type value ctx_map)))
      else
        raise (TypeError ((Printf.sprintf
          "Type mismatch: Targets %s do not match value type %s" 
          (String.concat ", " (List.map typ_to_str targets_types)) 
          (typ_to_str value_type)), location))



    | AnnAssign {location; target; annotation; value; _} ->

      let target_type = infer_type target ctx_map in
      let annotation_type = infer_type annotation ctx_map in
      let value_type = 
        match value with
        | Some v -> infer_type v ctx_map
        | None -> PSTC.Any in
      (* Check if target == annotation*)
      if target_type <> PSTC.Unset && target_type != annotation_type then
        raise (TypeError((Printf.sprintf "Type mismatch: Found %s, but should be %s" (typ_to_str target_type) (typ_to_str annotation_type)), location))
      (* Check if all of value (if defined), target and annotation are equal*)
      else if (assign value_type != PSTC.Unset) && (assign value_type != annotation_type) then
       raise (TypeError((Printf.sprintf "Value type mismatch: Found %s, but should be %s" (typ_to_str value_type) (typ_to_str annotation_type)), location))
      else if target_type == annotation_type then
        print_dbg location (Printf.sprintf "Correct Type Match: %s: %s = %s" (typ_to_str target_type) (typ_to_str annotation_type) (typ_to_str value_type))
      (* Add target to context if not already defined*)
      else (
        match target with
        | Name {id; _}->
            let id_str = PyreAst.Concrete.Identifier.to_string id in
            if Hashtbl.mem ctx_map id_str then
              raise (TypeError ((Printf.sprintf "Target already exists: %s" id_str), location))
            else
              let assigned_type = assign annotation_type in
              Hashtbl.add ctx_map id_str assigned_type;
              (* Print the added type *)
              print_dbg location (Printf.sprintf "Added type: %s <- %s" id_str (typ_to_str assigned_type))
        | _ -> 
          raise (NotImplemented "Target is not a Name\nAnnotation assignment is not fully implemented"))
    | _ -> print_dbg (get_location statement) (Printf.sprintf "Found other statement")
  ) with
  | NotImplemented msg-> Printf.printf "%sERROR%s: Not Implement Exception: %s" red reset_color msg
  | FunctionCallError (msg, loc)-> Printf.printf "%s%sERROR%s: %s\n" (loc_to_str loc) red reset_color msg
  | TypeError (msg, loc) -> Printf.printf "%s%sERROR%s: Type Error: %s\n" (loc_to_str loc) red reset_color msg
  ) statements

(* Main function to read a file and process its content *)
let read_file filepath =
  let ic = open_in filepath in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let () =
  let filepath = "test_inputs/inp1.py" in
  let content = read_file filepath in
  let ast = str_to_statements content in
  let ctx_map = Hashtbl.create 10 in
  process_statements ast ctx_map;
  (* You can add more processing logic here *)
  ()