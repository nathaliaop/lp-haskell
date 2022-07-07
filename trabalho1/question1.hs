maior4 :: Int -> Int -> Int -> Int -> Int
maior4 a b c d = maior2 (maior2 a b) (maior2 c d)
                 where
                 maior2 x y | x <= y = y
                            | otherwise = x

main :: IO ()
main = do
print(maior4 2 7 3 1)