exception UnclosedString
exception UnexpectedEscape
exception Unreachable of string

type token =
    | MapOpen
    | MapClose
    | ArrOpen
    | ArrClose
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null

type cursor =
    | InString of Buffer.t
    | InExpr of Buffer.t
    | Escaped of cursor (* previous state to return to after 1 character *)
    | Normal

let list_of_string s = List.of_seq (String.to_seq s)

let expr_of_string = function
    | "true" -> Bool true
    | "false" -> Bool false
    | "null" -> Null
    | s ->
            if String.exists ((=) '.') s then
                Float (float_of_string s)
            else
                Int (int_of_string s)

let char_escape = function
    | 'n' -> '\n'
    | c -> c

let tokens_of_string s =
    let rec tokens_of_string_i cursor acc cs =
        match cursor, cs with
        | Escaped _, [] -> raise UnexpectedEscape
        | InString _, [] -> raise UnclosedString
        | InExpr buf, [] -> expr_of_string (Buffer.contents buf) :: acc
        | Normal, [] -> acc
        | Escaped (InString buf | InExpr buf), c :: cs ->
                Buffer.add_char buf (char_escape c);
                tokens_of_string_i cursor acc cs
        | Escaped Normal, _ -> raise UnexpectedEscape
        | Escaped (Escaped _), _ -> raise (Unreachable "double escape")
        | InString buf, '\\' :: cs -> tokens_of_string_i (Escaped (InString buf)) acc cs
        | InString buf,  '"' :: cs -> tokens_of_string_i Normal (String (Buffer.contents buf) :: acc) cs
        | InString buf, c :: cs ->
                Buffer.add_char buf c;
                tokens_of_string_i (InString buf) acc cs
        | InExpr buf, ',' :: cs -> tokens_of_string_i Normal (expr_of_string (Buffer.contents buf) :: acc) cs
        | InExpr buf, ':' :: cs -> tokens_of_string_i Normal (expr_of_string (Buffer.contents buf) :: acc) cs
        | InExpr buf, '[' :: cs -> tokens_of_string_i Normal (ArrOpen :: expr_of_string (Buffer.contents buf) :: acc) cs
        | InExpr buf, ']' :: cs -> tokens_of_string_i Normal (ArrClose :: expr_of_string (Buffer.contents buf) :: acc) cs
        | InExpr buf, '{' :: cs -> tokens_of_string_i Normal (MapOpen :: expr_of_string (Buffer.contents buf) :: acc) cs
        | InExpr buf, '}' :: cs -> tokens_of_string_i Normal (MapClose :: expr_of_string (Buffer.contents buf) :: acc) cs
        | InExpr buf, ('\n' | ' ') :: cs -> tokens_of_string_i (InExpr buf) acc cs
        | InExpr buf, c :: cs ->
                Buffer.add_char buf c;
                tokens_of_string_i (InExpr buf) acc cs
        | Normal, (':' | ',') :: cs -> tokens_of_string_i Normal acc cs
        | Normal, '"' :: cs -> tokens_of_string_i (InString (Buffer.create 0)) acc cs
        | Normal, '[' :: cs -> tokens_of_string_i Normal (ArrOpen :: acc) cs
        | Normal, ']' :: cs -> tokens_of_string_i Normal (ArrClose :: acc) cs
        | Normal, '{' :: cs -> tokens_of_string_i Normal (MapOpen :: acc) cs
        | Normal, '}' :: cs -> tokens_of_string_i Normal (MapClose :: acc) cs
        | Normal, ('\n' | ' ') :: cs -> tokens_of_string_i Normal acc cs
        | Normal, c :: cs ->
                let buf = Buffer.create 0 in
                Buffer.add_char buf c;
                tokens_of_string_i (InExpr buf) acc cs
    in List.rev (tokens_of_string_i Normal [] (list_of_string s))
