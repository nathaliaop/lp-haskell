matrizTransposta :: Num u => [[u]] -> [[u]]
matrizTransposta [] = []
matrizTransposta a = (pegaPrimeiroElemento a):(matrizTransposta (retiraPrimeiroElemento a))
                     where
                     pegaPrimeiroElemento :: Num u => [[u]] -> [u]
                     pegaPrimeiroElemento [] = []
                     pegaPrimeiroElemento a = [ head f | f <- a, length f /= 0]

                     retiraPrimeiroElemento ([a]:_) = []
                     retiraPrimeiroElemento a = [drop 1 f | f <- a]

main :: IO ()
main =  do

print(matrizTransposta [[3,2,1], [1,2,3]])