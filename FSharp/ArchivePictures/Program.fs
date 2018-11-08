open System
open System.IO
open Domain

let movePictures source destination =
    let pictures =
        Directory.EnumerateFiles (source, "*.jpg", SearchOption.AllDirectories)
        |> Seq.map (fun s -> { FullPath = s; Name = Path.GetFileName s})
        |> Seq.map (fun f -> { File = f; TakenOn = DateTimeOffset(2018, 11, 8, 9, 51, 10, TimeSpan.FromHours 1.) })
    FileMover.move (FileSystem.FileSystemOperations ()) destination pictures
    |> Seq.iter id

[<EntryPoint>]
let main argv =
    match argv with
    | [|source; destination|] -> movePictures source destination
    | _ -> printfn "Please provide source and destination directories as arguments."
    0 // return an integer exit code
