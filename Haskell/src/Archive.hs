module Archive where

import Data.Time
import Text.Printf
import qualified Data.Map.Strict as Map
import Tree

data PhotoFile =
  PhotoFile { photoFileName :: FilePath, takenOn :: LocalTime }
  deriving (Eq, Show, Read)

moveTo :: (Foldable t, Ord a, PrintfType a) => a -> t PhotoFile -> Tree a FilePath
moveTo destination =
  Node destination . Map.foldrWithKey addDir [] . foldr groupByDir Map.empty
  where
    dirNameOf (LocalTime d _) =
      let (y, m, _) = toGregorian d in printf "%d-%02d" y m
    groupByDir (PhotoFile fileName t) =
      Map.insertWith (++) (dirNameOf t) [fileName]
    addDir name files dirs = Node name (Leaf <$> files) : dirs
