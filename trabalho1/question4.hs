histograma :: [String] -> [(String, Int)]
histograma [] = []
histograma a =  removeDuplicates [(ta, frequency ta a) | ta <- a]
                where
                removeDuplicates [] = []
                removeDuplicates ((a,b):as) | frequencyPair a as > 0 = removeDuplicates as
                                            | otherwise = (a,b):removeDuplicates as
                frequencyPair _ [] = 0
                frequencyPair s ((a,b):as) | s == a = 1 + frequencyPair s as
                                           | otherwise = frequencyPair s as
                frequency _ [] = 0
                frequency s (a:as) | s == a = 1 + frequency s as
                                   | otherwise = frequency s as                          

main :: IO ()
main =  do
print(histograma ["1","2","2","5","4","4"])        