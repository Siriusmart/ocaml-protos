type foldable =
    | Map of foldable list
    | Array of foldable list
    | MapOpen
    | MapClose
    | ArrOpen
    | ArrClose
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null

let unclean_of_tokens = List.map (
    function
        | Tokens.MapOpen -> MapOpen
        | Tokens.MapClose -> MapClose
        | Tokens.ArrOpen -> ArrOpen
        | Tokens.ArrClose -> ArrClose
        | Tokens.String s -> String s
        | Tokens.Int i -> Int i
        | Tokens.Float f -> Float f
        | Tokens.Bool b -> Bool b
        | Tokens.Null -> Null
)

let rec foldable_of_tokens ts =
    let rec seek_token target disallow = function
        | [] -> None
        | t :: ts ->
                if t = target then
                    Some ([], ts)
                else if List.mem t disallow then
                    None
                else
                    match seek_token target disallow ts with
                    | Some (content, trailing) -> Some (t :: content, trailing)
                    | None -> None
    in
    let rec seek_pair = function
        | [] -> None
        | ArrOpen :: ts ->
                begin
                    match seek_token ArrClose [ArrOpen; MapOpen; MapClose] ts with
                    | Some (content, trailing) -> Some ([], Array content, trailing)
                    | None ->
                        match seek_pair ts with
                        | Some (prec, content, trailing) -> Some (ArrOpen :: prec, content, trailing)
                        | None -> None
                end
        | MapOpen :: ts ->
                begin
                    match seek_token MapClose [ArrOpen; ArrClose; MapOpen] ts with
                    | Some (content, trailing) -> Some ([], Map content, trailing)
                    | None ->
                        match seek_pair ts with
                        | Some (prec, content, trailing) -> Some (MapOpen :: prec, content, trailing)
                        | None -> None
                end
        | t :: ts ->
                begin
                    match seek_pair ts with
                    | Some (prec, content, trailing) -> Some (t :: prec, content, trailing)
                    | None -> None
                end
    in
    match seek_pair ts with
    | Some (prec, content, trailing) -> foldable_of_tokens (prec @ [content] @ trailing)
    | None -> ts
