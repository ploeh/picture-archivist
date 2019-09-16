module Photo

open System
open System.IO
open System.Drawing
open System.Text
open System.Globalization

[<Literal>]
let private exifDateTaken = 0x0132

[<Literal>]
let private exifDateTimeOriginal = 0x9003

let private tryParseDate s =
    let res =
        DateTime.TryParseExact(
            s,
            "yyyy:MM:dd HH:mm:ss",
            CultureInfo.InvariantCulture,
            DateTimeStyles.None)
    match res with
    | true, dt -> Some dt
    | _ -> None

let extractDateTaken (fi : FileInfo) =
    let extractExif (img : Image) exif =
        if img.PropertyIdList |> Array.contains exif
        then
            let pi = img.GetPropertyItem exif
            Some (Encoding.ASCII.GetString(pi.Value, 0, pi.Len - 1))
        else None

    try
        use photo = Image.FromFile fi.FullName

        [ exifDateTimeOriginal; exifDateTaken ]
        |> Seq.choose (extractExif photo)
        |> Seq.tryHead
        |> Option.bind tryParseDate
    with
        | :? OutOfMemoryException -> None
        | :? FileNotFoundException -> None