namespace OpilioCraft.FSharp.Prelude

open System.IO

[<RequireQualifiedAccess>]
module FileSystem =
    let isRoot (fsi : FileSystemInfo) : bool = Path.GetPathRoot(fsi.Name) = fsi.Name

    let hasAttribute (attr : FileAttributes) (fsi : FileSystemInfo) : bool = (fsi.Attributes &&& attr) = attr
    let isDirectory     : FileSystemInfo -> bool = hasAttribute FileAttributes.Directory
    let isHiddenFile    : FileSystemInfo -> bool = hasAttribute FileAttributes.Hidden
    let isReadOnly      : FileSystemInfo -> bool = hasAttribute FileAttributes.ReadOnly
    let isSystemFile    : FileSystemInfo -> bool = hasAttribute FileAttributes.System
    let isTemporaryFile : FileSystemInfo -> bool = hasAttribute FileAttributes.Temporary

    let isEmpty (fi : FileInfo) : bool = fi.Length < int64 1
