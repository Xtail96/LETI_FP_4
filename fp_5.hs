f :: (a -> a) -> Int -> (a -> a)
f g n 
    | n > 1 = \x -> (f g (n-1)) $ g x
    | n == 1 = \x -> g x
    | otherwise = error "Exception: n must be positive number"
