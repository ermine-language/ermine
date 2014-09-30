module IO.CSV where

import File
import Function
import List
import String
import Control.Functor
import Syntax.IO

readCSVFile : FilePath -> IO (List (List String))
readCSVFile = map parseCSV . readFile

readCSVURL : URL       -> IO (List (List String))
readCSVURL  = map parseCSV . readURL

parseCSV : String -> List (List (String))
parseCSV = (fmap listFunctor $ split ',') . lines
