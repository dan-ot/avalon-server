module Avalon.Results

/// Lets us use computation expressions with Result.
type ResultBuilder() =
    member _.BindReturn(result, f) =
        result |> Result.map f

    member _.Bind(result, f) =
        result |> Result.bind f

    member _.Return(x) =
        Ok x

    member _.Zero() =
        Ok ()

let resultOf = new ResultBuilder()

let orFail result =
    match result with
    | Ok x -> x
    | Error e -> failwith e

let orError message input =
    match input with
    | Some x -> Ok x
    | None -> Error message