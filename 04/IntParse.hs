import Data.Char (digitToInt) -- we'll need ord shortly

asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                in loop acc' xs



asInt_fold :: String -> Int  
asInt_fold (x:xs) = 
    if x == '-' then
        foldl (\acc y-> acc*10 - digitToInt y) 0 xs
    else
        foldl (\acc y-> acc*10 + digitToInt y) 0 (x:xs)

concat_fold ::[[a]] -> [a]
concat_fold = foldr (\y acc-> y ++ acc) []


takeWhile'::(a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if f x then x: takeWhile' f xs else []


takeWhile_fold::(a -> Bool) -> [a] -> [a]
takeWhile_fold f  = foldr (\y acc -> if f y then y : acc else []) [] 