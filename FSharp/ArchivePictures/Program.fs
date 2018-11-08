open System
open System.IO
open Domain

let movePictures source destination =
    let pictures =
        Directory.EnumerateFiles (source, "*.jpg", SearchOption.AllDirectories)
        |> Seq.map (fun s -> { FullPath = s; Name = Path.GetFileName s})
        |> Seq.choose (fun f ->
            Photo.extractDateTaken (FileInfo f.FullPath)
            |> Option.map (fun dt -> { File = f; TakenOn = DateTimeOffset(dt, TimeSpan.Zero) }))
    FileMover.move (FileSystem.FileSystemOperations ()) destination pictures
    |> Seq.iter id

[<EntryPoint>]
let main argv =
    match argv with
    | [|source; destination|] -> movePictures source destination
    | _ -> printfn "Please provide source and destination directories as arguments."
    0 // return an integer exit code
