namespace OpilioCraft.FSharp.Prelude

open System.IO

[<RequireQualifiedAccess>]
module FileSystem =
    type FsiPredicate = FileSystemInfo -> bool
    let hasAttribute (attr: FileAttributes) (fsi: FileSystemInfo) : bool = (fsi.Attributes &&& attr) = attr

    let isRoot : FsiPredicate = fun fsi -> Path.GetPathRoot(fsi.Name) = fsi.Name

    let isDirectory : FsiPredicate = hasAttribute FileAttributes.Directory
    let isHiddenFile : FsiPredicate = hasAttribute FileAttributes.Hidden
    let isReadOnly : FsiPredicate = hasAttribute FileAttributes.ReadOnly
    let isSystemFile : FsiPredicate = hasAttribute FileAttributes.System
    let isTemporaryFile : FsiPredicate = hasAttribute FileAttributes.Temporary

    let isEmpty (fi: FileInfo) : bool = fi.Length < int64 1
