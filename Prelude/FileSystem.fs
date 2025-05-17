namespace OpilioCraft.FSharp.Prelude

open System.IO

[<RequireQualifiedAccess>]
module FileSystem =
    let isRoot (fsi: FileSystemInfo) = Path.GetPathRoot(fsi.Name) = fsi.Name

    type FileSystemInfoPredicate = FileSystemInfo -> bool
    let private hasAttribute (attr: FileAttributes) (fsi: FileSystemInfo) = (fsi.Attributes &&& attr) = attr

    let isDirectory : FileSystemInfoPredicate = hasAttribute FileAttributes.Directory
    let isHiddenFile : FileSystemInfoPredicate = hasAttribute FileAttributes.Hidden
    let isReadOnly : FileSystemInfoPredicate = hasAttribute FileAttributes.ReadOnly
    let isSystemFile : FileSystemInfoPredicate = hasAttribute FileAttributes.System
    let isTemporaryFile : FileSystemInfoPredicate = hasAttribute FileAttributes.Temporary

    let isEmpty (fi: FileInfo) = fi.Length < int64 1
