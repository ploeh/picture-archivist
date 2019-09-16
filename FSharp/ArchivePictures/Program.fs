open System.IO
open Ploeh.Samples

let rec readTree path =
    if File.Exists path
    then Leaf path
    else
        let dirsAndFiles = Directory.EnumerateFileSystemEntries path
        let branches = Seq.map readTree dirsAndFiles |> Seq.toList
        Node (path, branches)

let readPhoto file =
    Photo.extractDateTaken file
    |> Option.map (fun dateTaken -> { File = file; TakenOn = dateTaken })

let writeTree t =
    let copy m =
        Directory.CreateDirectory m.Destination.DirectoryName |> ignore
        m.Source.CopyTo m.Destination.FullName |> ignore
        printfn "Copied to %s" m.Destination.FullName
    let compareFiles m =
        let sourceStream = File.ReadAllBytes m.Source.FullName
        let destinationStream = File.ReadAllBytes m.Destination.FullName
        sourceStream = destinationStream
    let move m =
        copy m
        if compareFiles m then m.Source.Delete ()
    Tree.iter move t

let movePhotos source destination =
    let sourceTree = readTree source |> Tree.map FileInfo
    let photoTree = Tree.choose readPhoto sourceTree
    let destinationTree =
        Option.map (Archive.moveTo destination >> Archive.calculateMoves) photoTree
    Option.iter writeTree destinationTree

[<EntryPoint>]
let main argv =
    match argv with
    | [|source; destination|] -> movePhotos source destination
    | _ -> printfn "Please provide source and destination directories as arguments."
    0 // return an integer exit code
