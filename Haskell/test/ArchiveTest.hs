module ArchiveTest where

import Data.Time
import System.FilePath
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Tree
import Archive

archiveTests =
  [ testGroup "Archive tests" $ hUnitTestToTests $ TestList
    [
      "Move to destination" ~: do
        (source, destination, expected) <-
          [
            ( Leaf $ PhotoFile "1" $ lt 2018 11 9 11 47 17
            , "D"
            , Node "D" [Node "2018-11" [Leaf "1"]])
            ,
            ( Node "S" [
                Leaf $ PhotoFile "4" $ lt 1972 6 6 16 15 00]
            , "D"
            , Node "D" [Node "1972-06" [Leaf "4"]])
            ,
            ( Node "S" [
                Leaf $ PhotoFile "L" $ lt 2002 10 12 17 16 15,
                Leaf $ PhotoFile "J" $ lt 2007 4 21 17 18 19]
            , "D"
            , Node "D" [Node "2002-10" [Leaf "L"], Node "2007-04" [Leaf "J"]])
            ,
            ( Node "1" [
                Leaf $ PhotoFile "a" $ lt 2010 1 12 17 16 15,
                Leaf $ PhotoFile "b" $ lt 2010 3 12 17 16 15,
                Leaf $ PhotoFile "c" $ lt 2010 1 21 17 18 19]
            , "2"
            , Node "2" [
                Node "2010-01" [Leaf "a", Leaf "c"],
                Node "2010-03" [Leaf "b"]])
            ,
            ( Node "foo" [
                Node "bar" [
                  Leaf $ PhotoFile "a" $ lt 2010 1 12 17 16 15,
                  Leaf $ PhotoFile "b" $ lt 2010 3 12 17 16 15,
                  Leaf $ PhotoFile "c" $ lt 2010 1 21 17 18 19],
                Node "baz" [
                  Leaf $ PhotoFile "d" $ lt 2010 3 1 2 3 4,
                  Leaf $ PhotoFile "e" $ lt 2011 3 4 3 2 1
                ]]
            , "qux"
            , Node "qux" [
                Node "2010-01" [Leaf "a", Leaf "c"],
                Node "2010-03" [Leaf "b", Leaf "d"],
                Node "2011-03" [Leaf "e"]])
          ]
        let actual = moveTo destination source
        return $ expected ~=? actual
      ,
      "Calculate moves" ~: do
        (tree, expected) <-
          [
            (Leaf "1", Leaf $ Move "1" "1"),
            (Node "a" [Leaf "1"], Node "a" [Leaf $ Move "1" $ "a" </> "1"]),
            (Node "a" [Leaf "1", Leaf "2"], Node "a" [
              Leaf $ Move "1" $ "a" </> "1",
              Leaf $ Move "2" $ "a" </> "2"]),
            (Node "a" [Node "b" [Leaf "1", Leaf "2"], Node "c" [Leaf "3"]],
             Node "a" [
               Node ("a" </> "b") [
                 Leaf $ Move "1" $ "a" </> "b" </> "1",
                 Leaf $ Move "2" $ "a" </> "b" </> "2"],
               Node ("a" </> "c") [
                 Leaf $ Move "3" $ "a" </> "c" </> "3"]])
          ]
        let actual = calculateMoves tree
        return $ expected ~=? actual
    ]
  ]

lt y mth d h m s = LocalTime (fromGregorian y mth d) (TimeOfDay h m s)