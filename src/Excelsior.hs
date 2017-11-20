module Excelsior where

import Foundation

import Excelsior.Types.Args
import Excelsior.Misc.ExcelDirectoryManager

initialize :: Args -> IO ()
initialize args = case outputDirectory args of
    Nothing -> loadFiles (inputDirectory args) "out"
    Just fp -> loadFiles (inputDirectory args) (fp <> "/out")
