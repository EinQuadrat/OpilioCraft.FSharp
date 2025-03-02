module OpilioCraft.FSharp.Prelude.UserSettings

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open Microsoft.FSharp.Reflection

open OpilioCraft.FSharp.Prelude.IO

// errors
type ErrorReason =
    | InvalidUserSettings of File:string * ErrorMessage:string
    | IncompatibleVersion of Type:Type * Expected:Version * Found:Version
    | MissingProperty of Name:string
    | MissingFolder of Path:string
    | MissingFile of Path:string

// corresponding exceptions
exception UserSettingsException of ErrorReason

// needed json converters
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


// load user settings
let loadWithOptions<'T> jsonFilename jsonOptions =
    if File.Exists(jsonFilename)
    then
        try
            let settingsAsJson = File.ReadAllText(jsonFilename) in
            JsonSerializer.Deserialize<'T>(settingsAsJson, options = jsonOptions) |> Ok
        with
            | exn -> Error <| InvalidUserSettings(File = jsonFilename, ErrorMessage = exn.Message)
    else
        Error <| IncompleteSetup(jsonFilename)

let load<'T> jsonFilename = loadWithOptions<'T> jsonFilename (JsonSerializerOptions.Default)

let throwExceptionOnError = function
    | InvalidSetup(missingFile) -> raise (IncompleteSetupException(missingFile))
    | InvalidUserSettings(file, err) -> raise (InvalidUserSettingsException(file, err))
    | IncompatibleVersion(settingsType, expected, found) -> raise (IncompatibleVersionException(settingsType, expected, found))
    | MissingFolder(path) -> raise (IO.DirectoryNotFoundException(path))
    | MissingFile(path) -> raise (IO.FileNotFoundException(path))

// save
let saveWithOptions<'T> jsonFile jsonOptions settings =
    let json = JsonSerializer.Serialize<'T>(settings, options = jsonOptions) in
    saveGuard jsonFile <| fun uri -> File.WriteAllText(uri, json)

let save<'T> jsonFile settings = saveWithOptions<'T> jsonFile (JsonSerializerOptions.Default)

// supportive functions
let tryGetProperty name settings : 'T option =
    settings.GetType().GetProperty(name)
    |> Option.ofObj
    |> Option.map (fun prop -> prop.GetValue(box settings))
    |> Option.bind (function | :? 'T as typedValue -> Some typedValue | _ -> None)

let verify test onFail settings =
    match settings |> test with
    | true -> settings |> Ok
    | _ -> settings |> onFail |> Error

module Version =
    [<Literal>]
    let VersionPropertyName = "Version"

    let isValidVersion expectedVersion settings = tryGetProperty VersionPropertyName settings |> function
        | None ->
            Error <| IncompatibleVersion(Type = settings.GetType(), Expected = expectedVersion, Found = Version())
        | Some(foundVersion : Version) when foundVersion.CompareTo(expectedVersion) <> 0 ->
            Error <| IncompatibleVersion(Type = settings.GetType(), Expected = expectedVersion, Found = foundVersion)
        | _ ->
            Ok settings
