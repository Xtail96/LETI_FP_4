id_ :: a -> a
id_ x = x

eval :: (a -> b, a) -> b
eval (f,x) = f x

exchange :: (a, b) -> (b, a)
exchange (x,y) = (y,x)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \x -> f (g x)

curry_ :: ((a, b) -> c) -> (a -> b -> c)
curry_ f x y = f (x, y)

associate :: (a, (b, c)) -> ((a, b), c)
associate (x, (y, z)) = ((x, y), z)
