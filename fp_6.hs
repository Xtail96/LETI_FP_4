lastFibNum :: Int -> Int
lastFibNum 0 = 0
lastFibNum 1 = 1
lastFibNum n = ( lastFibNum(n-2) + lastFibNum(n-1) ) `mod` 10