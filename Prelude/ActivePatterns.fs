module OpilioCraft.FSharp.Prelude.ActivePatterns

open System.IO
open System.Text.RegularExpressions

open TryWrapper

let (|HeadOfArray|_|) theArray headValue =
    match theArray with
    | [| headValue |] -> Some (Array.tail theArray)
    | _ -> None

let (|IsFile|_|) (whatever : string) =
    match File.Exists(whatever) with
    | true -> Some(FileInfo(whatever))
    | _ -> None

let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern) in
    if m.Success then Some (m) else None

// active patterns for try-parsing strings
let (|IsBoolean|_|)         = parseBoolean
let (|IsDateTime|_|)        = parseDateTime
let (|IsDateTimeOffset|_|)  = parseDateTimeOffset
let (|IsDecimal|_|)         = parseDecimal
let (|IsDouble|_|)          = parseDouble
let (|IsInt32|_|)           = parseInt32
let (|IsSingle|_|)          = parseSingle
let (|IsTimeSpan|_|)        = parseTimeSpan
