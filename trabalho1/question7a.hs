somaMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
somaMatricial [] [] = []
somaMatricial (a:as) (b:bs) = somaLinha a b: somaMatricial as bs
                              where
                              somaLinha [] [] = []
                              somaLinha (a:as) (b:bs) = (a + b):somaLinha as bs

main :: IO ()
main =  do
print(somaMatricial [[3,2,1], [1,2,3]] [[3,4,5], [6,7,8]])