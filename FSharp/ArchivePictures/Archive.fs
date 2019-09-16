namespace Ploeh.Samples

open System
open System.IO

type PhotoFile = { File : FileInfo; TakenOn : DateTime }

type Move = { Source : FileInfo; Destination : FileInfo }

module Archive =
    let moveTo destination t =
        let dirNameOf (dt : DateTime) = sprintf "%d-%02d" dt.Year dt.Month
        let groupByDir pf m =
            let key = dirNameOf pf.TakenOn
            let dir = Map.tryFind key m |> Option.defaultValue []
            Map.add key (pf.File :: dir) m
        let addDir name files dirs =
            Tree.node name (List.map Leaf files) :: dirs

        let m = Tree.foldBack groupByDir t Map.empty
        Map.foldBack addDir m [] |> Tree.node destination

    let calculateMoves =
        let replaceDirectory (f : FileInfo) d =
            FileInfo (Path.Combine (d, f.Name))
        let rec imp path = function
            | Leaf x ->
                Leaf { Source = x; Destination = replaceDirectory x path }
            | Node (x, xs) ->
                let newNPath = Path.Combine (path, x)
                Tree.node newNPath (List.map (imp newNPath) xs)
        imp ""