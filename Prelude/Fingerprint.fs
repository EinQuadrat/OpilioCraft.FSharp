﻿namespace OpilioCraft.FSharp

module Fingerprint =
    open System.IO
    open System.Security.Cryptography
    open System.Text.RegularExpressions

    type QualifiedFingerprint =
        | Full of string
        | Partly of string
        | Derived of string
        | Unknown

        member x.Value =
            match x with
            | Full x | Partly x | Derived x -> x
            | Unknown -> invalidOp $"[{nameof QualifiedFingerprint}] cannot extract value of unknown fingerprint"

    // Algorithm used
    let private hashingAlgorithm = SHA256.Create();

    // Helper functions
    let private readStream (stream: FileStream) =
        seq {
            let mutable currentByte = 0

            let moveNext () =
                currentByte <- stream.ReadByte()
                currentByte >= 0

            while moveNext () do
                yield byte currentByte
        }

    let private readBytes path length =
        use stream = File.OpenRead(path)
        readStream stream |> Seq.truncate length |> Seq.toArray

    let private convertBytesToString (bytes: byte[]) =
        bytes |> Array.fold (fun resultString b -> resultString + b.ToString("x2")) ""
    
    // Fingerprint calculations
    let calculatePartlyFingerprint path =
        hashingAlgorithm.ComputeHash(readBytes path 1024)

    let calculateFingerprint path =
        use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, 16 * 1024 * 1024)
        hashingAlgorithm.ComputeHash(stream)

    // Converter functions
    let fingerprintAsString = calculateFingerprint >> convertBytesToString
    let partlyFingerprintAsString = calculatePartlyFingerprint >> convertBytesToString

    // Typed result
    let getFullFingerprint = fingerprintAsString >> Full
    let getPartlyFingerprint = partlyFingerprintAsString >> Partly

    // Guess fingerprint from filename
    [<Literal>]
    let FingerprintSeparator = "#"

    let private _fingerprintRegex = Regex(@"^(.+)#([0-9a-z]{64})$", RegexOptions.Compiled)

    let tryGuessFingerprint (path: string) : string option =
        let matchResult =
            Path.GetFileNameWithoutExtension(path)
            |> _fingerprintRegex.Match

        if matchResult.Success
        then
            Some(matchResult.Groups.[2].Value)
        else
            None

    // High-level API
    type FingerprintStrategy =
        | Calculate = 0
        | GuessFirst = 1

    let tryParseStrategy (input: string) : FingerprintStrategy option =
        match System.Enum.TryParse<FingerprintStrategy>(input, true) with
        | true, value -> Some(value)
        | _ -> None

    let getFingerprint (strategy: FingerprintStrategy) (filename: string) =
        match strategy with
        | FingerprintStrategy.GuessFirst -> tryGuessFingerprint filename
        | _ -> None

        |> Option.map Derived
        |> Option.defaultValue (getFullFingerprint filename)


module FingerprintExtension =
    let private getFilenameWithoutFingerprint (path: string) =
        let filename = System.IO.Path.GetFileNameWithoutExtension(path)

        match filename.LastIndexOf('#') with
        | -1 -> filename
        | found -> filename.Substring(0, found)

    type System.IO.Path with
        static member ContainsFingerprint(path: string) =
            Fingerprint.tryGuessFingerprint path |> Option.isSome

        static member GetFilenameWithoutFingerprint(path: string) =
            getFilenameWithoutFingerprint path

        static member InjectFingerprint(path: string, fingerprint: string) : string =
            let directory = System.IO.Path.GetDirectoryName(path)
            let filename = getFilenameWithoutFingerprint path
            let extension = System.IO.Path.GetExtension(path)

            let augmentedFilename = $"{filename}{Fingerprint.FingerprintSeparator}{fingerprint}{extension}"
            
            System.IO.Path.Combine(directory, augmentedFilename)
