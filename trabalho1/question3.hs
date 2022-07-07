isDecrescente:: [Int] -> Bool
isDecrescente [] = True
isDecrescente [_] = True
isDecrescente (a:b:as) | a > b = isDecrescente (b:as)
                | otherwise = False

main :: IO ()
main =  do
print(isDecrescente (7:6:5:4:3:2:1:[]))
print(isDecrescente (7:6:5:4:2:3:1:[]))