splitWith1 :: (a -> Bool) -> [a] -> [[a]]
splitWith1 f [] = []
splitWith1 f (x:xs)  = 
    if f x then 
        []:(splitWith1 f xs)
    else
        let temp = splitWith1 f xs
        in 
            if not (null temp) then 
                (x : head temp):tail temp
            else
                (x: []): []

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = filter  (not.null) $ splitWith1 f xs



wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
