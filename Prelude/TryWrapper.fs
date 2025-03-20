module OpilioCraft.FSharp.Prelude.TryWrapper

// convenient, functional TryParse wrappers returning option<'a>
let tryWrapper (tryFunction: string -> bool * _) = tryFunction >> function
    | true, v    -> Some v
    | false, _   -> None

// typed string parser
let parseBoolean        = tryWrapper System.Boolean.TryParse
let parseDateTime       = tryWrapper System.DateTime.TryParse
let parseDateTimeOffset = tryWrapper System.DateTimeOffset.TryParse
let parseDecimal        = tryWrapper System.Decimal.TryParse
let parseDouble         = tryWrapper System.Double.TryParse
let parseInt32          = tryWrapper System.Int32.TryParse
let parseSingle         = tryWrapper System.Single.TryParse
let parseTimeSpan       = tryWrapper System.TimeSpan.TryParse

// conditional dictionary access
let tryGetValue (key : 'a) (dict : System.Collections.Generic.Dictionary<'a,'b>) : 'b option =
    dict.TryGetValue(key)
    |> function
        | true, v    -> Some v
        | false, _   -> None
