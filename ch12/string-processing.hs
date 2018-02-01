module StringProcessing where

import Data.List.Split
import Data.List

notThe :: String -> Maybe String
notThe xs = 
    if xs == "the"
        then Nothing
        else Just xs


replaceThe :: String -> String
replaceThe [] = ""
replaceThe xs =
     case notThe (head splited) of
        Nothing -> "a " ++ replaceThe (intercalate " " $ tail splited)
        Just w  -> w ++ " " ++ replaceThe (intercalate " " $ tail splited)
        where splited = splitOn " " xs
