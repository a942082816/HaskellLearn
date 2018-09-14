lucky::(Integral a)=>a->String
lucky 7="LUCK"
lucky x="NONE"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)




maximum'::(Ord a)=>[a]->a
maximum' []=error "empty list"
maximum' [x]=x
maximum' (x:xs)=max x (maximum' xs)


fib::(Integral a)=>a->a
fib x
    |x<1 =error "need >0"
    |x==1 =1
    |x==2 =1
    |otherwise =fib (x-1)+fib (x-2)


applyTwice::(a->a)->a->a
applyTwice f x=f (f x)



abs'::(Num a,Ord a)=>a->a
abs' x
    |x<0 = (-x)
    |otherwise =x

zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' _ [] _=[]
zipWith' _ _ []=[]
zipWith' f (x:xs)(y:ys)=f x y:zipWith' f xs ys  


flip'::(a->b->c)->b->a->c
flip' f=g
    where g x y=f y x

map'::(a->b)->[a]->[b]
map' _ []=[]
map' f (x:xs)=f x:map f xs

filter'::(a->Bool)->[a]->[a]
filter' _ []=[]
filter' f (x:xs)
    |f x=x:filter' f xs
    |otherwise =filter' f xs

takeWhile'::(a->Bool)->[a]->[a]
takeWhile' _ []=[]
takeWhile' f (x:xs)
    |f x=x:takeWhile' f xs
    |otherwise =[]

chain::(Integral a)=>a->[a]
chain 1=[1]
chain x
    |even x =x:chain(x `div` 2)
    |odd x =x:chain(x*3+1) 
numLongChains::Int
numLongChains=length(filter'(\xs->length xs>15)(map chain[1..100]))


sum'::(Num a)=>[a]->a
sum' xs =foldl (+) 0 xs


elem'::(Eq a)=>a->[a]->Bool
elem' y ys=foldl (\acc x->if x==y then True else acc) False ys


sum''::(Num a,Ord a)=>a->a->a
sum'' x y
    |x==y =x
    |otherwise =y+sum'' x (y-1)




quicksort::(Ord a)=>[a]->[a]
quicksort []=[]
quicksort(x:xs)=
    let smallerSorted=quicksort [a|a<-xs,a<=x]
    
        biggerSorted=quicksort [a|a<-xs,a>x]
    in smallerSorted++[x]++biggerSorted


sum1'::(Num a) => [a]->a
sum1' [] = 0
sum1' (x:xs) = 1 + sum' xs


elem1::(Eq a) => a->[a]->Bool
elem1 _ [] = False
elem1 a (x:xs)
    | a == x = True
    |otherwise =  elem1 a xs

toList::a->[a]
toList a = a:[]

map1'::(a->b)->[a]->[b]
map1' f xs = foldl (\l a->l ++ toList (f a)) [] xs

map1::(a->b)->[a]->[b]
map1 f xs = foldr (\a l -> f a: l) [] xs  

reverse'::[a]->[a]
reverse' xs = foldl (\acc y->y:acc) [] xs

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
surface::Shape->Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = abs $ (x1 - x2) * (y1 - y2) 


data Person = Person {
    name::String,
    sex::String,
    phone::String
}deriving(Show)


import qualified Data.Map as Map

data LockerState = Taken|Free deriving(Show,Eq)
type Code = String
type LockerMap = Map.Map Int(LockerState, Code)