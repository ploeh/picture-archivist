module Main where

import System.Environment
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import Graphics.HsExif
import Data.Foldable
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Tree
import Archive

main :: IO ()
main = do
  args <- getArgs
  case args of
    [source, destination] -> movePhotos source destination
    _ -> putStrLn "Please provide source and destination directories as arguments."

movePhotos :: FilePath -> FilePath -> IO ()
movePhotos source destination = fmap fold $ runMaybeT $ do
  sourceTree <- lift $ readTree source
  photoTree <- MaybeT $ catMaybeTree <$> traverse readPhoto sourceTree
  let destinationTree = calculateMoves $ moveTo destination photoTree
  lift $ applyMoves destinationTree

readTree :: FilePath -> IO (Tree FilePath FilePath)
readTree path = do
  isFile <- doesFileExist path
  if isFile
    then return $ Leaf path
    else do
      dirsAndfiles <- listDirectory path
      let paths = fmap (path </>) dirsAndfiles
      branches <- traverse readTree paths
      return $ Node path branches

readPhoto :: FilePath -> IO (Maybe PhotoFile)
readPhoto path = do
  exifData <- parseFileExif path
  let dateTaken = either (const Nothing) Just exifData >>= getDateTimeOriginal
  return $ PhotoFile path <$> dateTaken

applyMoves :: Foldable t => t Move -> IO ()
applyMoves = traverse_ move
  where
    move m = copy m >> compareFiles m >>= deleteSource
    copy (Move s d) = do
      createDirectoryIfMissing True $ takeDirectory d
      copyFileWithMetadata s d
      putStrLn $ "Copied to " ++ show d
    compareFiles m@(Move s d) = do
      sourceBytes <- B.readFile s
      destinationBytes <- B.readFile d
      return $ if sourceBytes == destinationBytes then Just m else Nothing
    deleteSource           Nothing = return ()
    deleteSource (Just (Move s _)) = removeFile s