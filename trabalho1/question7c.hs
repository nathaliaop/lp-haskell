multiplicacaoMatricial :: Num u => [[u]] -> [[u]] -> [[u]]
multiplicacaoMatricial a b = [multiplicaLinhas f (matrizTransposta b) | f <- a]
                             where
                             multiplicaLinha [] [] = 0
                             multiplicaLinha (a:as) (b:bs) = (a*b) + multiplicaLinha as bs

                             multiplicaLinhas [] [] = []
                             multiplicaLinhas a b = [multiplicaLinha a x | x <- b]
                             
                             matrizTransposta [] = []
                             matrizTransposta a = (pegaPrimeiroElemento a):(matrizTransposta (retiraPrimeiroElemento a))

                             pegaPrimeiroElemento [] = []
                             pegaPrimeiroElemento a = [ head f | f <- a, length f /= 0]
        
                             retiraPrimeiroElemento ([a]:_) = []
                             retiraPrimeiroElemento a = [drop 1 f | f <- a]

main :: IO ()
main =  do

print(multiplicacaoMatricial [[2,3,1], [-1,0,2]] [[1,-2], [0,5], [4,1]])