module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import TreeTest

main :: IO ()
main = defaultMain treeTests