module Excelsior.Misc.ExcelDirectoryManager (loadFiles) where

import System.FilePath (FilePath, dropExtension)
import System.Directory (listDirectory,createDirectory, setCurrentDirectory,doesDirectoryExist)
import Control.Monad (unless)
import Foundation
import Foundation.Collection (mapM_)

import Excelsior.Misc.ExcelToCsv

setDirectory :: FilePath -> IO ()
setDirectory fp = do
  createDirectory fp
  setCurrentDirectory fp

fileNames :: [FilePath] -> [FilePath]
fileNames = filter (isSuffixOf ".xlsx")

loadFiles :: FilePath -> FilePath -> IO ()
loadFiles inputArg outputArg = do
    files <- listDirectory inputArg
    directoryExists <- doesDirectoryExist outputArg
    if directoryExists then setCurrentDirectory outputArg else setDirectory outputArg
    createDirectories inputArg $ fileNames files

createDirectories :: FilePath -> [FilePath] -> IO ()
createDirectories inputDir = mapM_ $ \x -> do
  let name = dropExtension x
  directoryExists <- doesDirectoryExist name
  unless directoryExists $ do
    createDirectory name
    setCurrentDirectory name
    loadXlsx $ inputDir <> x
    setCurrentDirectory ".."
