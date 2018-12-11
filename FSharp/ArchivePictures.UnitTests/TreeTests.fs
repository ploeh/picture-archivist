module TreeTests

open Xunit
open Swensen.Unquote
open Ploeh.Samples

type FilterTreeTestData () as this =
    inherit TheoryData<Tree<string, int>, int -> bool, Tree<string, int> option> ()

    do this.Add (Leaf 1, (fun _ -> false), None)
    do this.Add (Leaf 1, (fun _ -> true), Some (Leaf 1))
    do this.Add (Tree.node "a" [Leaf 1], (fun _ -> true), Some (Tree.node "a" [Leaf 1]))
    do this.Add (Tree.node "b" [Leaf 1], (fun _ -> false), Some (Tree.node "b" []))
    do this.Add (Tree.node "" [Leaf 1; Leaf 2], (fun x -> 1 < x), Some (Tree.node "" [Leaf 2]))
    do this.Add (
        Tree.node "" [
            Tree.node "" [
                Leaf 1;
                Leaf 4;
                Leaf 2];
            Tree.node "" [
                Leaf 3;
                Leaf 5]],
        (fun x -> 2 < x),
        Tree.node "" [
            Tree.node "" [
                Leaf 4];
            Tree.node "" [
                Leaf 3;
                Leaf 5]] |> Some)

[<Theory; ClassData(typeof<FilterTreeTestData>)>]
let ``Filter tree`` tree p expected =
    let actual = Tree.filter p tree
    expected =! actual