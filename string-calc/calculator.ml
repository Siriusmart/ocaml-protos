let rec eval expr =
    let eval_un op a =
        match op with
        | Tokens.Neg -> -. a
    in
    let eval_bin op a b =
        match op with
        | Tokens.Add -> a +. b
        | Tokens.Sub -> a -. b
        | Tokens.Mul -> a *. b
        | Tokens.Div -> a /. b
        | Tokens.Pow -> a ** b
    in
    match expr with
    | ExprTree.UnaryOperator (op, a) -> eval_un op (eval a)
    | ExprTree.BinaryOperator (op, a, b) -> eval_bin op (eval a) (eval b)
    | ExprTree.Value v -> v

let expr_of_string s =
    let tokens = Tokens.tokens_of_string s in
    let parenthesised = Intermediate .inter_expr_of_tokens (Intermediate .map_tokens_to_inter_expr tokens) in
    ExprTree.expr_of_inter_expr parenthesised

let eval_string s =
    eval (expr_of_string s)
