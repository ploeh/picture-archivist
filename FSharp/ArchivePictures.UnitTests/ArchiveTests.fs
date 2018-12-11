module ArchiveTests

open System
open System.IO
open Xunit
open Swensen.Unquote
open Ploeh.Samples

type MoveToDestinationTestData () as this =
    inherit TheoryData<Tree<string, PhotoFile>, string, Tree<string, string>> ()

    let photoLeaf name (y, mth, d, h, m, s) =
        Leaf { File = FileInfo name; TakenOn = DateTime (y, mth, d, h, m, s) }

    do this.Add (
        photoLeaf "1" (2018, 11, 9, 11, 47, 17),
        "D",
        Node ( "D", [Node ("2018-11", [Leaf "1"])]))
    do this.Add (
        Node ("S", [photoLeaf "4" (1972, 6, 6, 16, 15, 0)]),
        "D",
        Node ("D", [Node ("1972-06", [Leaf "4"])]))
    do this.Add (
        Node ("S", [
            photoLeaf "L" (2002, 10, 12, 17, 16, 15);
            photoLeaf "J" (2007, 4, 21, 17, 18, 19)]),
        "D",
        Node ("D", [
            Node ("2002-10", [Leaf "L"]);
            Node ("2007-04", [Leaf "J"])]))
    do this.Add (
        Node ("1", [
            photoLeaf "a" (2010, 1, 12, 17, 16, 15);
            photoLeaf "b" (2010, 3, 12, 17, 16, 15);
            photoLeaf "c" (2010, 1, 21, 17, 18, 19)]),
        "2",
        Node ("2", [
            Node ("2010-01", [Leaf "a"; Leaf "c"]);
            Node ("2010-03", [Leaf "b"])]))
    do this.Add (
        Node ("foo", [
            Node ("bar", [
                photoLeaf "a" (2010, 1, 12, 17, 16, 15);
                photoLeaf "b" (2010, 3, 12, 17, 16, 15);
                photoLeaf "c" (2010, 1, 21, 17, 18, 19)]);
            Node ("baz", [
                photoLeaf "d" (2010, 3, 1, 2, 3, 4);
                photoLeaf "e" (2011, 3, 4, 3, 2, 1)])]),
        "qux",
        Node ("qux", [
            Node ("2010-01", [Leaf "a"; Leaf "c"]);
            Node ("2010-03", [Leaf "b"; Leaf "d"]);
            Node ("2011-03", [Leaf "e"])]))

[<Theory; ClassData(typeof<MoveToDestinationTestData>)>]
let ``Move to destination`` source destination expected =
    let actual = Archive.moveTo destination source
    expected =! Tree.map string actual

type CalculateMovesTestData () as this =
    inherit TheoryData<Tree<string, FileInfo>, Tree<string, (string * string)>> ()

    do this.Add (Leaf (FileInfo "1"), Leaf ("1", "1"))
    do this.Add (
        Node ("a", [Leaf (FileInfo "1")]),
        Node ("a", [Leaf ("1", Path.Combine ("a", "1"))]))
    do this.Add (
        Node ("a", [Leaf (FileInfo "1"); Leaf (FileInfo "2")]),
        Node ("a", [
            Leaf ("1", Path.Combine ("a", "1"));
            Leaf ("2", Path.Combine ("a", "2"))]))
    do this.Add (
        Node ("a", [
            Node ("b", [
                Leaf (FileInfo "1");
                Leaf (FileInfo "2")]);
            Node ("c", [
                Leaf (FileInfo "3")])]),
        Node ("a", [
            Node (Path.Combine ("a", "b"), [
                Leaf ("1", Path.Combine ("a", "b", "1"));
                Leaf ("2", Path.Combine ("a", "b", "2"))]);
            Node (Path.Combine ("a", "c"), [
                Leaf ("3", Path.Combine ("a", "c", "3"))])]))

[<Theory; ClassData(typeof<CalculateMovesTestData>)>]
let ``Calculate moves`` tree expected =
    let actual = Archive.calculateMoves tree
    expected =! Tree.map (fun m -> (m.Source.ToString (), m.Destination.ToString ())) actual