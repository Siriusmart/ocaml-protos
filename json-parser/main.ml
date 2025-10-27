type operation =
    | Serialize
    | Parse

let rec lines_of_ch ch =
    try
        let line = input_line ch in
        line :: lines_of_ch ch
    with
    | End_of_file  -> []

let string_of_ch ch =
    String.concat "\n" (lines_of_ch ch)
;;

while true do
    let rec get_ch () =
        print_string "Enter the path to a JSON file: ";
        let path = read_line () in
        try
            open_in path
        with
        | e ->
                print_string "Could not open file: ";
                print_endline (Printexc.to_string e);
                get_ch ();
    in
    let content = string_of_ch (get_ch ()) in
    try
        let expr = Parser.expr_of_string content in
        print_endline (Expr.to_string expr)
    with
    | e ->
            print_string "Could not parse JSON: ";
            print_endline (Printexc.to_string e);
done
