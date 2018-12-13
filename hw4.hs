module Hw4 where

import Data.Maybe
import Data.List
import Data.Char

-- Task2
min_max :: Ord val => [val] -> Maybe (val, val)
min_max [] = Nothing
min_max (x:xs) = let mx = min_max xs
  in if isNothing(mx) 
    then Just (x,x) 
    else Just (min x (fst (fromJust mx)), max x (snd (fromJust mx)))


-- Task4
major_item :: Eq a => [a] -> Maybe(a) -> Int -> Maybe(a)
major_item x candidate confidence =
    if null x
        then if confidence > 0
            then candidate
            else Nothing
        else if confidence == 0
            then major_item (tail x) (Just (head x)) 1
            else if candidate == Just(head x)
                then major_item (tail x) candidate (confidence + 1)
                else major_item (tail x) candidate (confidence - 1)

majority :: Eq a => [a] -> Maybe(a)
majority x = major_item x Nothing 0
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
isPalindrome (x:xs) =
    if toUpper(x) /= toUpper(last xs)
    then False
    else isPalindrome(init xs)


-- Additional Task
data NestedList a = Elem a | List [NestedList a] deriving(Eq, Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = x : []
flatten (List xs) = concatMap flatten xs
