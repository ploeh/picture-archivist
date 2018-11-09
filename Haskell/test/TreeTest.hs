module TreeTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Tree

treeTests =
  [ testGroup "Tree tests" $ hUnitTestToTests $ TestList
    [
      "Filter tree" ~: do
        (tree, p, expected) <-
          [
            (Leaf 1, const False, Nothing),
            (Leaf 1, const True, Just $ Leaf 1),
            (Node "a" [Leaf 1], const True, Just $ Node "a" [Leaf 1]),
            (Node "b" [Leaf 1], const False, Just $ Node "b" []),
            (Node "" [Leaf 1, Leaf 2], (1 <), Just $ Node "" [Leaf 2]),
            (Node "" [
              Node "" [
                Leaf 1,
                Leaf 4,
                Leaf 2],
              Node "" [
                Leaf 3,
                Leaf 5]],
              (2 <),
             Just $ Node "" [
              Node "" [
                Leaf 4],
              Node "" [
                Leaf 3,
                Leaf 5]])
          ]
        let actual = filterTree p tree
        return $ expected ~=? actual
    ]
  ]