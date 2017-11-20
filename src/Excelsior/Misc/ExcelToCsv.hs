module Excelsior.Misc.ExcelToCsv (loadXlsx) where


import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text,unpack)
import Data.Map.Lazy (keys,lookup)

import Codec.Xlsx
import Foundation
import Foundation.IO (hPut, withFile, IOMode(WriteMode))
import Foundation.String (toBytes, Encoding (UTF8))
import qualified Foundation.VFS.FilePath as FP
import System.FilePath (FilePath)

import Excelsior.Misc.Utils (for)


loadXlsx :: FilePath -> IO ()
loadXlsx fp = do
  bs     <- L.readFile fp
  input  <- getWorksheets $ toXlsx bs
  forM_ input $ \(worksheetTitle, worksheetContent) ->
    case getXlsxValues $ worksheetValues worksheetContent of
      Nothing -> putStrLn "Skipping invalid worksheet.."
      Just ws -> writeCsv ws worksheetTitle

writeCsv :: [[Maybe Cell]] -> Text -> IO ()
writeCsv cells title = do
  let fileTitle = unpack title
  let output    = cells & cellToCellValue & parseCellValue & commaSeparator & unlineString
  withFile (fromString (fileTitle <> ".csv") :: FP.FilePath) WriteMode $ \handle -> hPut handle (toBytes UTF8 output)

cellToCellValue :: [[Maybe Cell]] -> [[CellValue]]
cellToCellValue cells = fmap (fmap (fromMaybe (CellText ""))) (cellToValues cells)

parseCellValue :: [[CellValue]] -> [[String]]
parseCellValue cellVal = fmap (drop 1) $ drop 1 $ fmap (fmap showCells) cellVal

commaSeparator :: [[String]] -> [String]
commaSeparator = fmap (intercalate ",")

showCells :: CellValue -> String
showCells (CellText text) = fromString $ unpack text
showCells (CellDouble double) = show double
showCells (CellBool bool) = show bool
showCells (CellRich richTextRun) = show richTextRun

unlineString :: [String] -> String
unlineString = intercalate "\n"

getWorksheets :: Xlsx -> IO [(Text, Worksheet)]
getWorksheets xlsx = return $ _xlSheets xlsx

worksheetValues :: Worksheet -> CellMap
worksheetValues = _wsCells

numberOfRows :: CellMap -> Maybe Int
numberOfRows = fmap maximum . nonEmpty . fmap fst . keys

numberOfColumns :: CellMap -> Maybe Int
numberOfColumns = fmap maximum . nonEmpty . fmap fst . keys

getXlsxValues :: CellMap -> Maybe [[Maybe Cell]]
getXlsxValues cm = do
  rows <- numberOfRows cm
  cols <- numberOfColumns cm
  return $ for [0..rows] $ \r ->
              for [0..cols] $ \c ->
                  lookup (r, c) cm

cellToValues :: [[Maybe Cell]] -> [[Maybe CellValue]]
cellToValues = fmap (fmap (>>= _cellValue))
