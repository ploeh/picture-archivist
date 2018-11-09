module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import TreeTest
import ArchiveTest

main :: IO ()
main = defaultMain (archiveTests ++ treeTests)