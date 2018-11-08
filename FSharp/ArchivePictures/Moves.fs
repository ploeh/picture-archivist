module Moves

open Domain
open Paths

let private getMoveRequest basePath (picture: Picture) =
    let destinationFolder =
        pathCombine basePath picture.formatTakenOn

    let destination =
        pathCombine destinationFolder picture.File.Name

    {Source=picture.File.FullPath; Destination=destination}

let getMoveRequests basePath (pictures:seq<Picture>) =
    pictures
    |> Seq.map (fun picture -> getMoveRequest basePath picture)