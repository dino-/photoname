module Photoname.Serial (
   getSerial
)
   where

import System.IO
import System.Process
import Text.ParserCombinators.Parsec

{- Later on this will get the serial directly from the camera-assigned 
   filename. This looking inside the file is kind of a hack.
-}


{- Quick and dirty function to run a process and grab its output.
   This evil thing doesn't watch STDERR at all or otherwise do anything
   even remotely safe.
   XXX Move this somewhere logical like Photoname.Util
-}
getProcessOutput :: FilePath -> [String] -> IO String
getProcessOutput path args = do
   (_, outH, _, _) <- runInteractiveProcess path args Nothing Nothing
   hGetContents outH


findSerial :: String -> Maybe String
findSerial s =
   case (parse parser "" s) of
      Left _  -> Nothing
      Right x -> Just x
   where
      parser = do
         manyTill anyChar $ try (string "Image Number: ")
         count 3 digit
         char '-'
         digit
         count 3 digit


getSerial :: FilePath -> IO (Maybe String)
getSerial p = do
   allOutput <- getProcessOutput "exiftags" [p]
   let maybeSerial = findSerial allOutput
   return maybeSerial
