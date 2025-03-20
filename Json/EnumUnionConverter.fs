namespace OpilioCraft.FSharp.Json

open System
open System.Text.Json
open System.Text.Json.Serialization

open Microsoft.FSharp.Reflection

type EnumUnionConverter<'T> () =
    inherit JsonConverter<'T> ()

    let cases = FSharpType.GetUnionCases(typeof<'T>) |> Array.map (fun case -> case.Name, case) |> Map.ofArray

    override _.Read (reader : byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) =
        let rawCase = reader.GetString()

        try
            let casePrototype = cases.[rawCase]
            FSharpValue.MakeUnion(casePrototype, args = [| |]) :?> 'T
        with
            | :? Collections.Generic.KeyNotFoundException -> failwith $"[{nameof(EnumUnionConverter)}] \"{rawCase}\" is not a valid case for {typeof<'T>.Name}"

    override _.Write (writer: Utf8JsonWriter, value: 'T, _: JsonSerializerOptions) =
        writer.WriteStringValue(value.ToString())
