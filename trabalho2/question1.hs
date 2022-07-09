comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q [] = [(m,q)]
comprarMedicamento m q (e:es) | not (existe m (e:es)) = (m,q):(e:es)
                              | (fst(e) == m) = (m,q + snd(e)):es 
                              | otherwise = e:comprarMedicamento m q es
                              where
                              existe m [] = False
                              existe m (l:ls) | (fst(l) /= m) = existe m ls
                                             | (fst(l) == m) = True

main :: IO ()
main =  do
print(comprarMedicamento "batata" 2 [])
print(comprarMedicamento "batata" 2 [("cenoura", 3), ("batata", 2), ("feijao", 3)])
print(comprarMedicamento "batata" 2 [("cenoura", 2), ("feijao", 3)])