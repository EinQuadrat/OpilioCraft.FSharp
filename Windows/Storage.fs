module OpilioCraft.FSharp.Windows.Storage

open System
open Windows.Storage

open OpilioCraft.FSharp.Text

// ----------------------------------------------------------------------------

let getFolders (current: StorageFolder) : StorageFolder seq =
    current.GetFoldersAsync().GetAwaiter().GetResult()

let getMatchingFolders (matcher: string -> bool) (current: StorageFolder) : StorageFolder list =
    getFolders current |> Seq.filter (_.Name >> matcher) |> Seq.toList

let matchExact pattern value = (value = pattern)
let matchLike pattern value = vbLike value pattern

let tryGetMatchingFolders (matcher: string -> bool) (current: StorageFolder) =
    getMatchingFolders matcher current
    |> function [] -> None | folderList -> Some folderList

// path resolution, returns all matching folders
let resolvePath (parent: StorageFolder) (path: string) : StorageFolder list =
    let rec resolvePathRecursive (current: StorageFolder) (pathParts: string list) : StorageFolder list =
        match pathParts with
        | [] -> [ current ]
        | pathPart :: tail ->
            current
            |> getMatchingFolders (vbLike pathPart)
            |> List.collect (fun subfolder -> resolvePathRecursive subfolder tail)

    if (String.IsNullOrEmpty(path) || path = "/")
    then
        [ parent ]
    else
        path.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> resolvePathRecursive parent

// path navigation, returns single matching folder or raises exception
let tryNavigate (parent: StorageFolder) (path: string) : StorageFolder option =
    resolvePath parent path
    |> function [ oneMatchingFolder ] -> Some oneMatchingFolder | _ -> None
