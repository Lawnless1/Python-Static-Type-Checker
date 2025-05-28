let () = print_endline "Inference Started ..."


exception NotImplemented of string
(* exception ParseError of string *)

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
    | List t1 -> "list[" ^ typ_to_str t1 ^ "]"
    | Tuple ts ->
        "tuple[" ^ (typ_to_str ts) ^ "]"
    | Dict (k, v) ->
        "dict[" ^ typ_to_str k ^ ", " ^ typ_to_str v ^ "]"
    | Set t1 -> "set[" ^ typ_to_str t1 ^ "]"
    | Frozenset t1 -> "frozenset[" ^ typ_to_str t1 ^ "]"
    | Union ts ->
        "Union[" ^ String.concat ", " (List.map typ_to_str ts) ^ "]"
    | Function (args, ret) ->
        "Function[" ^
        String.concat ", " (List.map typ_to_str args) ^
        " -> " ^ typ_to_str ret ^ "]"
    | Lambda (args, ret) ->
        "Lambda[" ^
        String.concat ", " (List.map typ_to_str args) ^
        " -> " ^ typ_to_str ret ^ "]"
    | Any -> "Any"
    | Class name -> "class \"" ^ name ^ "\""
    | Unset -> "Unset"

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

let infer_type (expr: PyreAst.Concrete.Expression.t) (ctx_map : (string, PSTC.typ) Hashtbl.t) : PSTC.typ =
  let open PyreAst.Concrete.Expression in
  match expr with
  | Name {id; ctx; _} ->
      (match ctx with
        | Load -> id_to_typ id
        | Store -> 
          let id_str = PyreAst.Concrete.Identifier.to_string id in
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
  | _ -> raise (NotImplemented ("Type inference for this expression is not yet implemented"))

let rec process_statements (statements: (PyreAst.Concrete.Statement.t list)) (ctx_map : (string, PSTC.typ) Hashtbl.t): unit =
  let open PyreAst.Concrete.Statement in
  List.iter (function statement ->
    match statement with
    | FunctionDef { name; body; _ } ->
        Printf.printf "Found function: %s\n" (PyreAst.Concrete.Identifier.to_string name);
        process_statements body ctx_map
    | ClassDef { name; body; _ } ->
        Printf.printf "Found class: %s\n" (PyreAst.Concrete.Identifier.to_string name);
        process_statements body ctx_map
    | Assign {targets; value; _} ->
        Printf.printf "Assign: %s <- %s\n" 
            (String.concat ", " (List.map (fun target -> typ_to_str (infer_type target ctx_map)) targets))
          (typ_to_str (infer_type value ctx_map));
    | AnnAssign {target; annotation; _} ->
      let target_type = infer_type target ctx_map in
      let annotation_type = infer_type annotation ctx_map in
      if target_type <> PSTC.Unset && target_type != annotation_type then
        Printf.printf "Type mismatch: Found %s, but should be %s\n" (typ_to_str target_type) (typ_to_str annotation_type)
      else if target_type == annotation_type then
        Printf.printf "Annotation: %s = %s\n" (typ_to_str target_type) (typ_to_str annotation_type)
      else (
        match target with
        | Name {id; _}->
            let id_str = PyreAst.Concrete.Identifier.to_string id in
            if Hashtbl.mem ctx_map id_str then
              Printf.printf "Target already exists: %s\n" id_str
            else
              Hashtbl.add ctx_map id_str annotation_type;
              Printf.printf "Added type: %s <- %s\n" id_str (typ_to_str annotation_type)
        | _ -> Printf.printf "Target is not a Name\n")
    | _ -> Printf.printf "Found other statement\n"
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