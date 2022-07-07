consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento m [] = 0
consultarMedicamento m (e:es) | (fst(e) == m) = snd(e)
                              | otherwise = consultarMedicamento m es

main :: IO ()
main =  do
print(consultarMedicamento "batata" [("cenoura", 3), ("feijao", 3)])
print(consultarMedicamento "batata" [])
print(consultarMedicamento "batata" [("cenoura", 3), ("batata", 2), ("feijao", 3)])