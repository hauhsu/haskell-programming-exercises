module ValidateTheWord where

newtype Word' =
    Word' String 
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs = 
    let numVowel = 3 in
    if (length . filter (==True) $ map isVowel xs) > numVowel
        then Nothing
        else Just (Word' xs)
    where isVowel x = elem x vowels
    
