module ExprUtil where

-- | a function to split lists modified slightly from <https://stackoverflow.com/a/22594891>
splits :: [a] -> [([a],[a])]
splits xx = zipWith splitAt [1..((length xx)-1)] (repeat xx)

-- | a function to flat map stuff taken from <https://stackoverflow.com/questions/2986787/haskell-flatmap>
flatMap :: (t -> [a]) -> [t] -> [a]
flatMap _ [] = []
flatMap f (x:xs) = f x ++ flatMap f xs