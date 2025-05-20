let () = print_endline "Hello, World"

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


  let rec process_statements (statements: (PyreAst.Concrete.Statement.t list)) : unit =
  List.iter (function
    | PyreAst.Concrete.Statement.FunctionDef { name; body; _ } ->
        Printf.printf "Found function: %s\n" (PyreAst.Concrete.Identifier.to_string name);
        process_statements body
    | PyreAst.Concrete.Statement.ClassDef { name; body; _ } ->
        Printf.printf "Found class: %s\n" (PyreAst.Concrete.Identifier.to_string name);
        process_statements body
    | _ -> Printf.printf "Found other statement\n"
  ) statements


  let () =
    let text = "def Hello():\n    pass" in
    let ast = str_to_statements text in
    process_statements ast;
    (* You can add more processing logic here *)
    ()