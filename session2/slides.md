% Haskell working group 
% session 2
% Rémi and Dhananjay

# Recap

# Types

~~~{haskell}
42 :: Int
42.0 :: Float
'a' :: Char
[1, 2] :: [Int] -- List of Ints
"Hello" :: [Char] -- :'(
length :: [a] -> Int
~~~~

# Functions

~~~{haskell}
map :: (a -> b) -> [a] -> [b]
map f (x:xs) = (f s):map f xs

map (+1) [1,2]
-- returns [2,3]
~~~

# Some more functions

~~~{haskell}
-- fold
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- to add up a list
sum2 :: Int -> Int -> Int
sum2 a b = a+b

sumList :: [Int] -> Int
sumList li = foldl (sum2) 0 li
~~~

# Creating New Data types
## Algebraic Data types
Create new from current..

## Start with caps!!

# Enumeration Types

~~~{haskell}
data Fruit = Banana | Apple | Orange
             deriving (Show, Eq)
~~~

# No more null BS

~~~{haskell}
Apple :: Fruit
NoFruit :: Fruit
~~~

~~~{haskell}
Prelude> NoFruit :: Fruit

<interactive>:19:1: Not in scope: data constructor ‘NoFruit’

<interactive>:19:12:
    Not in scope: type constructor or class ‘Fruit’
~~~

# Embedding Results

~~~{haskell}
data Result = Failure | OK Double
                       deriving (Show)
~~~

# Making in more generic
~~~{haskell}
data Result a = Failure | OK a
~~~

# SafeDiv
~~~{haskell}
safeDiv2 :: Double -> Double -> Result2 Double
~~~

# DataTypes - Recap
~~~{haskell}
data Name = Constructor1 type11 type12 ...
        | Constructor2 type21 ..
        | Constructor3
~~~

# DataTypes in Functions
~~~{haskell}
showResult :: Result -> String
~~~

# Pattern matching

# case statements
~~~{haskell}
case something of
  case1 -> result1
  case2 -> result2
  ...
~~~

# Log file parsing

