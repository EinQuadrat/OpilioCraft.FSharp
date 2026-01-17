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

let vbLike (pattern: string) (text: string) =
    // Source - adapted for correctness and escaping
    if pattern = null then false
    elif pattern = "" then text = ""
    else
        let sb = StringBuilder()
        let mutable insideList = false
        let mutable prevInsideList = false
        let patLen = pattern.Length

        for i in 0 .. patLen - 1 do
            let c = pattern.[i]
            let tempInsideList = insideList

            // Manage pattern start: do not anchor if pattern starts with '*'
            if i = 0 && c <> '*' then
                sb.Append('^') |> ignore

            if c = '[' && not insideList then
                insideList <- true
                sb.Append('[') |> ignore
            elif c = ']' && insideList then
                insideList <- false
                sb.Append(']') |> ignore
            elif c = '!' && insideList && not prevInsideList then
                // '!' as first char inside a character class means negation in VB-like -> '^' in regex
                sb.Append('^') |> ignore
            elif not insideList then
                match c with
                | '?' -> sb.Append('.') |> ignore
                | '#' -> sb.Append(@"\d") |> ignore
                | '*' when (i = 0 || i = patLen - 1) -> () // leading/trailing '*' suppresses anchors instead of being explicitly added
                | '*' -> sb.Append(".*") |> ignore
                | _ ->
                    // Escape any regex metacharacter outside character classes
                    sb.Append(System.Text.RegularExpressions.Regex.Escape(c.ToString())) |> ignore
            else
                // inside character class: append raw character except escape backslash
                if c = '\\' then sb.Append(@"\\") |> ignore
                else sb.Append(c) |> ignore

            prevInsideList <- tempInsideList

        // Manage end anchor: if pattern does not end with '*' then anchor end
        if patLen > 0 && pattern.[patLen - 1] <> '*' then
            sb.Append('$') |> ignore

        let regexPattern = sb.ToString()
        System.Text.RegularExpressions.Regex.IsMatch(text, regexPattern)