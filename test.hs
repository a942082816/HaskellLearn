doubleMe x=x+x

max'::(Ord a) => a->a->a
max' a b
    | a>b = a
    | otherwise = b


maximum'::(Ord a)=>[a]->a
maximum' []  = error "list is empty"
maximum' [x] = x
maximum' (x:xs)
    | x> maximum' xs =x
    | otherwise =maximum' xs



replicate'::(Num i,Ord i)=>i->a->[a]
replicate' n x
    | n<=0 =[]
    |otherwise =x:replicate' (n-1) x


take'::(Num i,Ord i)=>i->[a]->[a]
take' i []=[]
take' i (x:xs)
    |i<=0 =[]
    |otherwise =x:take' (i-1) xs

reverse'::[a]->[a]
reverse' [] = []
reverse' (x:xs) =(reverse' xs) ++[x]


repeat'::a->[a]
repeat' a = a:repeat' a


zip'::[a]->[b]->[(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) =(x,y):zip' xs ys


elem'::(Eq a)=>a->[a]->Bool
elem' _ [] =False
elem' a (x:xs) 
    |x==a =True
    |otherwise =elem' a xs


elem1::(Eq a)=>a->[a]->Bool
elem1 x xs=foldl(\acc y->if x==y then True else acc) False xs


quicksort::(Ord a)=>[a]->[a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSort =quicksort [a | a <- xs, a <= x] 
        biggerSort = quicksort [a | a <- xs, a > x]
    in smallerSort ++ [x] ++ biggerSort


applyTwice::(a->a)->a->a
applyTwice f x= f (f x)


zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) =f x y: zipWith f xs ys


flip'::(a->b->c)->(b->a->c)
flip' f a b=f b a


test::a->b->a
test a b = a


map'::(a->b)->[a]->[b]
map' _ [] =[]
map' f (x:s) =(f x):map' f s


map1::(a->b)->[a]->[b]
map1 f xs=foldr(\y acc->f y:acc) [] xs


filter'::(a->Bool)->[a]->[a]
filter' _ [] =[]
filter' f (x:xs)
    | f x = x:filter' f xs
    |otherwise = filter' f xs


div'::(Num a, Fractional a)=>a->a->a
div' x y = x / y


avg'::(Num a,Fractional a)=>[a]->Maybe a
avg' [] = Nothing
avg' xs = Just ret 
    where ret = (sum xs) / (fromIntegral $ length xs)



toPlalindrome::[a]->[a]
toPlalindrome xs = xs ++ reverse' xs


isPlalindrome::(Eq a)=>[a]->Bool
isPlalindrome [] = True
isPlalindrome xs = xs == reverse' xs


intersperse :: a -> [[a]] -> [a]
intersperse c [] = []
intersperse c (x:[]) = x ++ intersperse c []
intersperse c (x:xs) = x ++ (c:intersperse c xs)


