type inter_expr_unclean =
    | UnaryOperator of Tokens.un_op
    | BinaryOperator of Tokens.bin_op
    | Value of float
    | Expr of inter_expr_unclean list
    | Open
    | Close

let map_tokens_to_inter_expr =
    List.map (
        function
            | Tokens.BinaryOperator op -> BinaryOperator op
            | Tokens.UnaryOperator op -> UnaryOperator op
            | Tokens.Value v -> Value v
            | Tokens.Open -> Open
            | Tokens.Close -> Close
    )

let rec inter_expr_of_tokens (ts: inter_expr_unclean list) =
    let rec seek_closing = function
        | [] -> None
        | Open :: _ -> None
        | Close :: ts -> Some ([], ts)
        | t :: ts ->
                match seek_closing ts with
                | Some (content, trailing) -> Some (t :: content, trailing)
                | None -> None
    in
    let rec seek_opening_and_closing = function
        | [] -> None
        | Open :: ts ->
                begin
                    match seek_closing ts with
                    | Some (content, trailing) -> Some ([], content, trailing)
                    | None ->
                            match seek_opening_and_closing ts with
                            | Some (before, content, trailing) -> Some (Open :: before, content, trailing)
                            | None -> None
                end
        | t :: ts ->
                match seek_opening_and_closing ts with
                | Some (before, content, trailing) -> Some (t :: before, content, trailing)
                | None -> None
    in
    match seek_opening_and_closing ts with
    | None -> ts
    | Some (before, content, trailing) ->
            inter_expr_of_tokens (before @ [Expr (inter_expr_of_tokens content)] @ trailing)
