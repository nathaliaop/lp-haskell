import Data.List
import Data.Function

aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia x = sortBy (compare `on` snd) (map calculaMedia (filter (\(b,a,_) -> a >= 5)  x))
                        where
                        calculaMedia (a,b,c) = (a, (b + c) / 2)

main :: IO ()
main =  do
print(aprovadosOrdemDeMedia [("Joao", 2.3, 1.5), ("Maria", 7.0, 9.8), ("Zalvaro", 5.3, 9.8)])