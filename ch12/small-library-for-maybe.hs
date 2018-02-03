module MaybeLib where

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False


 -- >>> isNothing (Just 1)
 -- False
 -- >>> isNothing Nothing
 -- True
isNothing :: Maybe a -> Bool
isNothing x = not . isJust $ x

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
-- (the first argument is the default value)
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d _ Nothing = d
mayybee _ f (Just x)  = f x

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
-- Try writing it in terms
-- of the maybe catamorphism
fromMaybe :: a -> Maybe a -> a 
fromMaybe d x = mayybee d id x

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)


-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing  : xs) = catMaybes xs
catMaybes ((Just x) : xs) = x : catMaybes xs

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs =
    if any isNothing xs
        then Nothing
        else Just (catMaybes xs)
