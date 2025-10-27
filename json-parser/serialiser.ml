
let rec serialize = function
    | Expr.Null -> "null"
    | Expr.Float f -> string_of_float f
    | Expr.Int i -> string_of_int i
    | Expr.Array es ->
            let buf = Buffer.create 0 in
            Buffer.add_char buf '[';
            Buffer.add_string buf (String.concat "," (List.map serialize es));
            Buffer.add_char buf ']';
            Buffer.contents buf
    | Expr.Bool true -> "true"
    | Expr.Bool false -> "false"
    | Expr.String s ->
            let escape = function
                | '\n' -> "\\n"
                | '\\' -> "\\\\"
                | c -> String.make 1 c
            in
            let buf = Buffer.create 0 in
            Buffer.add_char buf '"';
            Buffer.add_string buf (String.concat "" (List.map escape (List.of_seq (String.to_seq s))));
            Buffer.add_char buf '"';
            Buffer.contents buf
    | Expr.Map ps ->
            let serialize_pair (s, v) =
                let buf = Buffer.create 0 in
                Buffer.add_char buf '"';
                Buffer.add_string buf (serialize (Expr.String s));
                Buffer.add_char buf '"';
                Buffer.add_char buf ':';
                Buffer.add_string buf (serialize v);
                Buffer.contents buf
            in
            let buf = Buffer.create 0 in
            Buffer.add_char buf '{';
            Buffer.add_string buf (String.concat "," (List.map serialize_pair ps));
            Buffer.add_char buf '}';
            Buffer.contents buf
