tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento m (e:es) | (fst(e) == m) && snd(e) > 1 = Just ((m, snd(e) - 1):es)
                          | (fst(e) == m) && snd(e) == 1 = Just es
                          | otherwise = case (tomarMedicamento m es) of
                                                   Just n -> Just (e:n)
                                                   Nothing -> Nothing

main :: IO ()
main =  do
print(tomarMedicamento "batata" [("cenoura", 3), ("feijao", 3)])
print(tomarMedicamento "batata" [])
print(tomarMedicamento "batata" [("cenoura", 3), ("batata", 2), ("feijao", 3)])