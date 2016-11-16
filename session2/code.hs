module Session2 where

{-
-- enumeration data type
data Fruit = Banana
  | Apple
  | Orange
  deriving (Show)

data Result = Failure
  | OK Double
  deriving (Show)

data Result2 a = Failure2
  | OK2 a
  deriving (Show)

safeDiv :: Double -> Double -> Result
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)


-- safeDiv2 :: Double -> Double -> Result2 Double

showResult :: Result -> String
showResult Failure = "oh jeez, the computation failed"
showResult (OK a) = "the result is: " ++ show a

showResult2 :: Result -> String
showResult2 res = case res of
  Failure -> "oh jeez, the computation failed!"
  (OK a) -> "the result is: " ++ show a
-}
