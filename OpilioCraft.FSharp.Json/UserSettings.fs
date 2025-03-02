module OpilioCraft.FSharp.Json.UserSettings

open System
open System.IO
open System.Text.Json

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
        Error <| MissingFile(Path = jsonFilename)

let load<'T> jsonFilename = loadWithOptions<'T> jsonFilename (JsonSerializerOptions.Default)

let throwExceptionOnError = fun err -> raise <| UserSettingsException err

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
