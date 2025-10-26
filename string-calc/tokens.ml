exception UnknownUnaryOperator of char
exception UnknownBinaryOperator of char

type bin_op =
    | Add
    | Sub
    | Mul
    | Div
    | Pow

type un_op =
    | Neg

type token =
    | UnaryOperator of un_op
    | BinaryOperator of bin_op
    | Open
    | Close
    | Value of float

let unary_of_char = function
    | '-' -> Neg
    | c -> raise (UnknownUnaryOperator c)

let binary_of_char = function
    | '+' -> Add
    | '-' -> Sub
    | '*' -> Mul
    | '/' -> Div
    | '^' -> Pow
    | c -> raise (UnknownBinaryOperator c)

let tokens_of_string s =
    let incomplete_value = ref None in
    let acc = ref [] in
    let flush_incomplete () =
        match !incomplete_value with
        | Some buf ->
                let value = float_of_string (Buffer.contents buf) in
                incomplete_value := None;
                acc := (Value value) :: !acc
        | None -> ()
    in
    let token_is_end_of_expr = function
        | Value _ -> true
        | Close -> true
        | _ -> false
    in
    let add_char c =
        match c with
        | ' ' -> ()
        | '0' .. '9' | '.' ->
                begin
                    match !incomplete_value with
                    | Some buf -> Buffer.add_char buf c
                    | None ->
                            let buffer = Buffer.create 1 in
                            Buffer.add_char buffer c;
                            incomplete_value := Some (buffer)
                end
        | '(' ->
                flush_incomplete ();
                acc := Open :: !acc
        | ')' ->
                flush_incomplete ();
                acc := Close :: !acc
        | c ->
                begin
                    flush_incomplete ();
                    if List.is_empty !acc || not (token_is_end_of_expr (List.hd !acc)) then
                        acc := (UnaryOperator (unary_of_char c)) :: !acc
                    else
                        acc := (BinaryOperator (binary_of_char c)) :: !acc
                end
    in String.iter add_char s; flush_incomplete (); List.rev !acc

let precedence_of_bin_op = function
    | Add -> 2
    | Sub -> 2
    | Mul -> 4
    | Div -> 4
    | Pow -> 4
