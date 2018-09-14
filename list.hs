import Data.List
import Data.Map

search ::(Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nLen = length needle
    in foldl (\acc x -> if take nLen x==needle then True else acc) False (tails haystack)


findKey::(Eq k)=>k->[(k,v)]-> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs) = 
    if k == key then 
        Just v
    else
        findKey key xs


findKey'::(Eq k)=>k->[(k,v)]->Maybe v
findKey' key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing