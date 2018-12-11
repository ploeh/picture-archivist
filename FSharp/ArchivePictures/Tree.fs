namespace Ploeh.Samples

type Tree<'a, 'b> = Node of 'a * Tree<'a, 'b> list | Leaf of 'b

module Tree =
    let leaf = Leaf
    
    let node x xs = Node (x, xs)

    let rec cata fd ff = function
        | Leaf x -> ff x
        | Node (x, xs) -> xs |> List.map (cata fd ff) |> fd x

    let choose f =
        cata (fun x -> List.choose id >> node x >> Some) (f >> Option.map Leaf)

    let filter p = choose (Some >> Option.filter p)

    let bimap f g = cata (f >> node) (g >> leaf)

    let bifold f g z t =
        let flip f x y = f y x
        cata (fun x xs -> flip f x >> List.fold (>>) id xs) (flip g) t z

    let bifoldBack f g t z =
        cata (fun x xs -> List.foldBack (<<) xs id >> f x) g t z

    let map f = bimap id f

    let fold f = bifold (fun x _ -> x) f

    let foldBack f = bifoldBack (fun _ x -> x) f

    let iter f = fold (fun () x -> f x) ()