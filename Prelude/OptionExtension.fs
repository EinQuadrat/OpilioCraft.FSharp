namespace OpilioCraft.FSharp.Prelude

// simplify Option handling
[<RequireQualifiedAccessAttribute>]
module Option =
    let ifNone action maybe =
        if Option.isNone maybe then action ()
        maybe

    let ofResult = function
        | Ok result -> Some result
        | Error _ -> None

    let tee action = function
        | Some value as maybe -> action value ; maybe
        | None -> None

    let filterOrElseWith (predicate : 'T -> bool) (elseAction : 'T -> unit) = function
        | Some value as opt -> if predicate value then opt else (elseAction value ; None)
        | None -> None
