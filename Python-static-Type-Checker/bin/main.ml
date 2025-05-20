let () = print_endline "Inference Started ..."

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

let infer_type (expr: PyreAst.Concrete.Expression.t) (ctx_map : (string, string) Hashtbl.t) : string =
  let open PyreAst.Concrete.Expression in
  match expr with
  | Name {id; ctx; _} ->
      (match ctx with
        | Load -> PyreAst.Concrete.Identifier.to_string id
        | Store -> 
          let id_str = PyreAst.Concrete.Identifier.to_string id in
          if Hashtbl.mem ctx_map id_str then
            Hashtbl.find ctx_map id_str
          else
            "Unset"
        | Del -> "Del"
      )
  | Constant {value; _} ->
      (match value with
        |None -> "None"
        | False -> "bool"
        | True -> "bool"
        | Ellipsis -> "Ellipsis"
        | Integer _ -> "int"
        | BigInteger _ -> "int"
        | Float _ -> "float"
        | Complex _ -> "complex"
        | String _ -> "str"
        | ByteString _ -> "bytes"
      )
  | _ -> "not implemented"

let rec process_statements (statements: (PyreAst.Concrete.Statement.t list)) (ctx_map : (string, string) Hashtbl.t): unit =
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
            (String.concat ", " (List.map (fun target -> infer_type target ctx_map) targets))
          (infer_type value ctx_map);
    | AnnAssign {target; annotation; _} ->
      let target_type = infer_type target ctx_map in
      let annotation_type = infer_type annotation ctx_map in
      if target_type <> "Unset" && target_type <> annotation_type then
        Printf.printf "Type mismatch: Found %s, but should be %s\n" target_type annotation_type
      else if target_type = annotation_type then
        Printf.printf "Annotation: %s = %s\n" target_type annotation_type
      else (
        match target with
        | Name {id; _}->
            let id_str = PyreAst.Concrete.Identifier.to_string id in
            if Hashtbl.mem ctx_map id_str then
              Printf.printf "Target already exists: %s\n" id_str
            else
              Hashtbl.add ctx_map id_str annotation_type;
              Printf.printf "Added type: %s <- %s\n" id_str annotation_type 
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