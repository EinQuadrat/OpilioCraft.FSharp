module OpilioCraft.FSharp.Prelude.Text

open System.Text

let trim (stringValue : string) =
    stringValue.Trim()

let tokenizeString (input : string) =
    let chars = input.Trim().ToCharArray() |> Array.toList

    let rec tokenizer remaining delim insideWord (word : StringBuilder option) wordList : string list =
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
