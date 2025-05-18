module OpilioCraft.FSharp.Magic

open System.IO

type FileType =
    | FT_UNKNOWN
    | FT_RAW
    | FT_BMP
    | FT_JPEG
    | FT_PNG
    | FT_GIF of string (* version number '87a' or '89a' *)
    | FT_Java_ByteCode
    | FT_Java_ByteCode_Compressed
    | FT_PostScript
    | FT_PDF
    | FT_MSDOS_EXE
    | FT_TIFF of string (* byte order 'LittleEndian' or 'BigEndian' *)
    | FT_MSOffice
    | FT_PKZip

let (|JPEG|_|) (data: byte list) =
    match data with
    | 0xFFuy :: 0xD8uy :: _ -> Some()
    | _ -> None

let (|PNG|_|) (data: byte list) =
    match data with
    | 0x89uy :: 'P'B :: 'N'B :: 'G'B :: 0x0Duy :: 0x0Auy :: 0x1Auy :: 0x0Auy :: _ -> Some()
    | _ -> None

let guessFileType (fileStream: FileStream) : FileType =
    let data : byte [] = Array.zeroCreate 256
    fileStream.Read(data, 0, data.Length) |> ignore
    fileStream.Position <- 0L // reset position immediately

    // Try some patterns
    match data |> Array.toList with
    | 0xFFuy :: 0xD8uy :: _ ->
        FT_JPEG

    | 0x89uy :: 'P'B :: 'N'B :: 'G'B :: 0x0Duy :: 0x0Auy :: 0x1Auy :: 0x0Auy :: _ ->
        FT_PNG

    | 'B'B :: 'M'B :: _ ->
        FT_BMP

    | 'G'B :: 'I'B :: 'F'B :: tail ->
        match tail with
        | '8'B :: '7'B :: 'a'B :: _ ->
            FT_GIF("87a")
        | '8'B :: '9'B :: 'a'B :: _ ->
            FT_GIF("89a")
        | _ -> FT_UNKNOWN

    | 0xCAuy :: 0xFEuy :: 0xBAuy :: 0xBEuy :: _ ->
        FT_Java_ByteCode

    | 0xCAuy :: 0xFEuy :: 0xD0uy :: 0x0Duy :: _ ->
        FT_Java_ByteCode_Compressed

    | 0x25uy :: 0x21uy :: _ ->
        FT_PostScript

    | 0x25uy :: 0x50uy :: 0x44uy :: 0x46uy :: _ ->
        FT_PDF

    | 'M'B :: 'Z'B :: _
    | 'Z'B :: 'M'B :: _ ->
        FT_MSDOS_EXE

    | 'I'B :: 'I'B :: 0x2Auy :: 0x00uy :: _ ->
        FT_TIFF("LittleEndian")

    | 'M'B :: 'M'B :: 0x00uy :: 0x2Auy :: _ ->
        FT_TIFF("BigEndian")

    | 0xD0uy :: 0xCFuy :: 0x11uy :: 0xE0uy :: _ ->
        FT_MSOffice

    | 'P'B :: 'K'B :: _ ->
        FT_PKZip

    | _ ->
        FT_UNKNOWN

