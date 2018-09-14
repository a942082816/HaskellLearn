
data BookInfo = Book Int String deriving(Show)


data List a = Cons a (List a)|Nil deriving(Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons a xa) = a:(toList xa)
toList Nil = []


data Tree a= Node a (Tree a) (Tree a) | Empty deriving (Show)



highTree::(Num b) => Tree a -> b
highTree Empty = 0
highTree (Node n l r) = 1 + max (highTree l) (highTree r)
