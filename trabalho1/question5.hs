myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] [] = []
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith binary (a:as) (b:bs) = (binary a b):(myZipWith binary as bs)

main :: IO ()
main =  do
putStr "ans = "
print(myZipWith mod [1, 2, 3] [3, 2, 1])