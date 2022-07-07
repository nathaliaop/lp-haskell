converterNotaParaMencao :: Float -> String
converterNotaParaMencao nota | nota >= 9 && nota <= 10 = "SS"
                             | nota >= 7 && nota < 9 = "MS"
                             | nota >= 5 && nota < 7 = "MM"
                             | nota >= 3 && nota < 5 = "MI"
                             | nota >= 0.1 && nota < 3 = "II"
                             | nota == 0 = "SR"
                             | otherwise = "Nota invalida"

main :: IO ()
main = do
print(converterNotaParaMencao 10)