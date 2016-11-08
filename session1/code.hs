
module Session1 where


import Prelude hiding
    ( concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , product
    , reverse
    , sum
    )

main :: IO ()
main = putStrLn "Hello World!"


length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs


sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs


product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ ret [] = ret
foldr f ret (x:xs) = f x (foldr f ret xs)


foldl :: (a -> b -> b) -> b -> [a] -> b
foldl _ ret [] = ret
foldl f ret (x:xs) = foldl f (f x ret) xs

-- sum = foldr (+) 0
-- product = foldr (*) 1

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) =
    if p x
       then x : filter p xs
       else filter p xs


sumOdds :: [Int] -> Int
sumOdds = sum . filter odd


reverse :: [a] -> [a]
reverse = go []
    where
        go acc [] = acc
        go acc (x:xs) = go (x : acc) xs


reverse2 :: [a] -> [a]
reverse2 = foldl (:) []


concat :: [a] -> [a] -> [a]
concat [] xs = xs
concat xs [] = xs
concat (x:xs) xs2 = x : concat xs xs2
