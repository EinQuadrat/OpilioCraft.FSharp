namespace OpilioCraft.FSharp.PowerShell

[<RequireQualifiedAccess>]
module Assert =
    let fileExists errorMessage path =
        if not <| System.IO.File.Exists path then failwith $"{errorMessage}: path"
        path
