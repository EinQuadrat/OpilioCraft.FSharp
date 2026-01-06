module OpilioCraft.FSharp.Text

open System.Text

let trim (stringValue: string) =
    stringValue.Trim()

let tokenizeString (input: string) =
    let chars = input.Trim().ToCharArray() |> Array.toList

    let rec tokenizer remaining delim insideWord (word: StringBuilder option) wordList : string list =
        match remaining with
        | ' ' :: rest when not insideWord -> // ignore spaces outside strings
            tokenizer rest delim false None wordList
        | c :: rest when insideWord && c = delim -> // delimiter found inside word --> end of word
            tokenizer rest ' ' false None (List.append wordList [word.Value.ToString()])
        | c :: rest ->
            match c with
            | '"' -> // new word found
                let word = new StringBuilder() in
                tokenizer rest '"' true (Some(word)) wordList
            | cc when not insideWord -> // new word found
                let word = new StringBuilder(cc.ToString()) in
                tokenizer rest ' ' true (Some(word)) wordList
            | cc -> // consume it
                tokenizer rest delim true (Some(word.Value.Append(cc))) wordList            
        | _ ->
            match word with
            | Some word -> List.append wordList [word.ToString()]
            | _ -> wordList
            
    tokenizer chars ' ' false None []

let vbLike (text: string) (pattern: string) =
    // Source - https://stackoverflow.com/a
    // Posted by Tom
    // Retrieved 2025-12-29, License - CC BY-SA 4.0

    let resultPattern = StringBuilder()
    let mutable insideList = false
    let mutable prevInsideList = false

    for i in 0 .. pattern.Length - 1 do
        let c = pattern.[i]
        let tempInsideList = insideList

        // Manage pattern start
        if i = 0 && c <> '*' then
            resultPattern.Append('^') |> ignore
        // Manage characterlists
        if c = '[' && not insideList then
            insideList <- true
            resultPattern.Append(c) |> ignore
        elif c = ']' && insideList then
            insideList <- false
            resultPattern.Append(c) |> ignore
        elif c = '!' && insideList && not prevInsideList then
            // Special chars for Like that need to be converted
            resultPattern.Append('^') |> ignore
        elif c = '?' && not insideList then
            resultPattern.Append('.') |> ignore
        elif c = '#' && not insideList then
            resultPattern.Append(@"\d") |> ignore
        elif c = '*' && i = 0 then
            // Nothing to append
            ()
        else
            resultPattern.Append(c) |> ignore

        prevInsideList <- tempInsideList

    let regexPattern = resultPattern.ToString()
    System.Text.RegularExpressions.Regex.IsMatch(text, regexPattern)