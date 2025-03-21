namespace OpilioCraft.FSharp.Prelude

// simplify Result handling
[<RequireQualifiedAccessAttribute>]
module Result =
    let toOption = function
        | Ok result -> Some result
        | Error _ -> None

    let ofOption errorReason = function
        | Some x -> Ok x
        | None -> Error errorReason

    let transform (okCase : 'a -> Result<'A,'B>) (errorCase : 'b -> Result<'A,'B>) : Result<'a,'b> -> Result<'A,'B> =
        function
        | Ok a -> okCase a
        | Error b -> errorCase b

    let tee okEffect errorEffect : Result<'a,'b> -> Result<'a,'b> =
        function
        | Ok a as result -> okEffect a ; result
        | Error b as result -> errorEffect b ; result

    let teeOk effect = tee effect ignore
    let teeError effect = tee ignore effect

    let test condition errorOnFail = Result.bind ( fun x -> condition x |> function | true -> Ok(x) | _ -> Error(errorOnFail) )
    let testWith condition onFail = Result.bind ( fun x -> condition x |> function | true -> Ok(x) | _ -> Error <| onFail x )
