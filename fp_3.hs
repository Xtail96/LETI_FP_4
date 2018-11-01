numSum :: Int -> Int
numSum 0 = 0
numSum a = let b = abs(a)
    in b `mod` 10 + numSum(b `div` 10)

numCount :: Int -> Int
numCount a = let b = abs(a) `div` 10
    in if b == 0 then 1 else numCount(b) + 1
