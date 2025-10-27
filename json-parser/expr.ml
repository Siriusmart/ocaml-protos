exception InvalidMapSyntax
exception MismatchParentheses

type expr =
    | Map of (string * expr) list
    | Array of expr list
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null

let rec to_string = function
    | Map ts -> String.concat "" ["Map ["; String.concat "; " (List.map (fun (k, v) -> String.concat "" ["("; to_string (String k); ", "; to_string v; ")"]) ts); "]"]
    | Array ts -> String.concat "" ["Array ["; String.concat "; " (List.map to_string ts); "]"]
    | String s ->
            let escape = function
                | '\n' -> "\\n"
                | '\\' -> "\\\\"
                | c -> String.make 1 c
            in
            String.concat "" ["String (\""; String.concat "" (List.map escape (List.of_seq (String.to_seq s))); "\")"]
    | Int i -> String.concat "" ["Int ("; string_of_int i; ")"]
    | Float f -> String.concat "" ["Int ("; string_of_float f; ")"]
    | Bool b -> String.concat "" ["Int ("; string_of_bool b; ")"]
    | Null -> "Null"

let rec chunk_map = function
    | [] -> []
    | String k :: v :: fs -> (k, v) :: chunk_map  fs
    | _ -> raise InvalidMapSyntax

let rec expr_of_foldable = function
    | Fold.Null -> Null
    | Fold.Bool b -> Bool b
    | Fold.Float f -> Float f
    | Fold.Int i -> Int i
    | Fold.String s -> String s
    | Fold.Array es -> Array (List.map expr_of_foldable es)
    | Fold.Map es -> Map (chunk_map (List.map expr_of_foldable es))
    | _ -> raise MismatchParentheses
