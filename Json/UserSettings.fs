module OpilioCraft.FSharp.Json.UserSettings

open System
open System.IO
open System.Text.Json

open OpilioCraft.FSharp.IO

// errors
type UserSettingsError =
    | InvalidUserSettingsError of File:string * ErrorMessage:string
    | IncompatibleVersionError of Type:Type * Expected:Version * Found:Version
    | MissingPropertyError of Name:string
    | MissingFolderError of Path:string
    | MissingFileError of Path:string

// corresponding exceptions
exception UserSettingsException of UserSettingsError

// load user settings
let loadWithOptions<'T> jsonFilename jsonOptions : Result<'T, UserSettingsError> =
    if File.Exists(jsonFilename)
    then
        try
            let settingsAsJson = File.ReadAllText(jsonFilename) in
            JsonSerializer.Deserialize<'T>(settingsAsJson, options = jsonOptions) |> Ok
        with
            | exn -> Error <| InvalidUserSettingsError(File = jsonFilename, ErrorMessage = exn.Message)
    else
        Error <| MissingFileError(Path = jsonFilename)

let load<'T> jsonFilename = loadWithOptions<'T> jsonFilename (JsonSerializerOptions.Default)

let throwExceptionOnError : UserSettingsError -> UserSettingsException = raise << UserSettingsException

// save
let saveWithOptions<'T> jsonFile jsonOptions settings =
    let json = JsonSerializer.Serialize<'T>(settings, options = jsonOptions) in
    saveGuard jsonFile (fun uri -> File.WriteAllText(uri, json))

let save<'T> jsonFile = saveWithOptions<'T> jsonFile (JsonSerializerOptions.Default)

// supportive functions
let tryGetProperty name settings : 'T option =
    settings.GetType().GetProperty(name)
    |> Option.ofObj
    |> Option.map (fun prop -> prop.GetValue(box settings))
    |> Option.bind (function | :? 'T as typedValue -> Some typedValue | _ -> None)

module Version =
    [<Literal>]
    let VersionPropertyName = "Version"

    let isValidVersion expectedVersion settings =
        tryGetProperty VersionPropertyName settings
        |> function
            | None ->
                Error <| IncompatibleVersionError(Type = settings.GetType(), Expected = expectedVersion, Found = Version())
            | Some (foundVersion : Version) when foundVersion.CompareTo(expectedVersion) <> 0 ->
                Error <| IncompatibleVersionError(Type = settings.GetType(), Expected = expectedVersion, Found = foundVersion)
            | _ ->
                Ok settings
