exception Unreachable
exception MismatchParentheses

type expr =
    | UnaryOperator of Tokens.un_op * expr
    | BinaryOperator of Tokens.bin_op * expr * expr
    | Value of float

let rec expr_of_inter_expr (inter_expr: Intermediate.inter_expr_unclean list) =
    match inter_expr with
    | [ Intermediate.Value v ] -> Value v
    | [ Intermediate.Expr inter_expr ] -> expr_of_inter_expr inter_expr
    | [ Intermediate.UnaryOperator u; inter_expr ] -> UnaryOperator (u, expr_of_inter_expr [inter_expr])
    | inter_expr ->
        let bin_ops = List.filter (function | Intermediate.BinaryOperator _ -> true | _ -> false) inter_expr in
        let bin_ops = List.map (function | Intermediate.BinaryOperator op -> op | _ -> raise Unreachable) bin_ops in
        let bin_precedences = List.map Tokens.precedence_of_bin_op bin_ops in
        let min_precedence = List.fold_left min max_int bin_precedences in
        let split_i_rev = List.find_index (function | Intermediate.BinaryOperator op -> Tokens.precedence_of_bin_op op = min_precedence | _ -> false) (List.rev inter_expr) in
        match split_i_rev with
        | None -> raise MismatchParentheses
        | Some i ->
                let i = List.length inter_expr - i - 1 in
                let left = List.take i inter_expr in
                let right = List.drop (i + 1) inter_expr in
                match List.nth inter_expr i with
                | Intermediate.BinaryOperator op -> BinaryOperator (op, expr_of_inter_expr left, expr_of_inter_expr right)
                | _ -> raise Unreachable
