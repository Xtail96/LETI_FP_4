module Hw4 where

import Data.Maybe
import Data.List
import Data.Char

-- Task2
minMax :: Ord val => [val] -> Maybe (val, val)
minMax [] = Nothing
minMax (x:xs) = let mx = minMax xs
  in if isNothing(mx) 
    then Just (x,x) 
    else Just (min x (fst (fromJust mx)), max x (snd (fromJust mx)))


-- Task4
helper :: Eq a => [a] -> Maybe(a) -> Int -> Maybe(a)
helper x candidate confidence =
    if null x
        then if confidence > 0
            then candidate
            else Nothing
        else if confidence == 0
            then helper (tail x) (Just (head x)) 1
            else if candidate == Just(head x)
                then helper (tail x) candidate (confidence + 1)
                else helper (tail x) candidate (confidence - 1)

majorItem :: Eq a => [a] -> Maybe(a)
majorItem x = helper x Nothing 0
-- source: https://habr.com/post/167177/

-- Task 6
lastFibNum :: Int -> Int
lastFibNum 0 = 0
lastFibNum 1 = 1
lastFibNum n = ( lastFibNum(n-2) + lastFibNum(n-1) ) `mod` 10

-- Task7
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
    | toUpper(x) /= toUpper(last xs) = False
    | otherwise = isPalindrome(init xs)


-- Additional Task
data NestedList a = Elem a | List [NestedList a] deriving(Eq, Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = x : []
flatten (List xs) = concatMap flatten xs
