module OpilioCraft.FSharp.Prelude.IO

open System
open System.IO

[<Literal>]
let BackupExtension = ".bak"

let saveGuard (file : string) (plainSaveOperation : string -> unit) =
    let needsBackup = if File.Exists(file) then Some(file + BackupExtension) else None

    match needsBackup with
    | Some backupFile ->
        try
            File.Move(file, backupFile)
        with
            | exn -> failwith $"[Guarded IO] cannot create backup file: {exn.Message}"

    | None -> ignore ()

    let errorOccurred : System.Exception option =
        try
            file |> plainSaveOperation
            None
        with
            | exn ->
                Console.Error.WriteLine $"[Guarded IO] error occurred while writing content to file {file}: {exn.Message}"
                Console.Error.WriteLine $"[Guarded IO] trying to restore previous file version from backup..."
                Some exn

    match errorOccurred, needsBackup with
    | Some _, Some backupFile ->
        try
            File.Move(backupFile, file)
        with
            | exn -> failwith $"[Guarded IO] cannot recover from IO error: {exn.Message}"

    | Some exn, None ->
        failwith $"[Guarded IO] cannot save changes to file: {exn.Message}"

    | None, Some backupFile ->
        try
            File.Delete(backupFile)
        with
            | exn -> Console.Error.WriteLine $"[Guarded IO] -warning- cannot cleanup backup file: {exn.Message}"

    | _ -> ignore()
