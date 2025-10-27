exception InvalidJSON

let tokens_of_string = Tokens.tokens_of_string

let foldable_of_string ts =
    Fold.foldable_of_tokens (Fold.unclean_of_tokens (tokens_of_string ts))

let expr_of_string s =
    match foldable_of_string s with
    | [ expr ] -> Expr.expr_of_foldable expr
    | _ -> raise InvalidJSON
