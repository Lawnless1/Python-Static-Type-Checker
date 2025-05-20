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
    let open PyreAst.Concrete.Statement in
    List.iter (function
      | FunctionDef { name; body; _ } ->
          Printf.printf "Found function: %s\n" (PyreAst.Concrete.Identifier.to_string name);
          process_statements body
      | ClassDef { name; body; _ } ->
          Printf.printf "Found class: %s\n" (PyreAst.Concrete.Identifier.to_string name);
          process_statements body
      | Assign {targets; value; type_comment; _} ->
          Printf.printf "Found assignment to: %s\n" targets;
      | _ -> Printf.printf "Found other statement\n"
    ) statements

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
    process_statements ast;
    (* You can add more processing logic here *)
    ()