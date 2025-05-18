namespace OpilioCraft.FSharp

// align usage with F# core library
[<RequireQualifiedAccessAttribute>]
module Option =
    let ifNone action opt =
        if Option.isNone opt then action ()
        opt

    let ofResult = function
        | Ok result -> Some result
        | Error _ -> None

    let tee f = function
        | Some value as opt -> f value ; opt
        | None -> None

    let teeP predicate f = tee (fun value -> if predicate value then f value)

    let filterOrElseWith (predicate: 'T -> bool) (elseAction: 'T -> unit) = function
        | Some value as opt -> if predicate value then opt else (elseAction value ; None)
        | None -> None
