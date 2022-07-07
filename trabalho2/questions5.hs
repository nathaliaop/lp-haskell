receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido ((m,h):ps) | (medicamentoRepetido m ps || horarioRepetido h || not (horarioOrdenado h) || not (medicamentoOrdenado ((m,h):ps))) = False
                             | otherwise = receituarioValido ps
                         where
                         medicamentoRepetido m [] = False
                         medicamentoRepetido m (l:ls) | m == fst(l) = True
                                                      | otherwise = medicamentoRepetido m ls
                         repeticaoNaLista h [] = False
                         repeticaoNaLista h (l:ls) | (h == l) = True
                                            | otherwise = repeticaoNaLista h ls
                         horarioRepetido [] = False
                         horarioRepetido (h:hs) | repeticaoNaLista h hs = True
                                                | otherwise = horarioRepetido hs
                         horarioOrdenado [] = True
                         horarioOrdenado [_] = True
                         horarioOrdenado (pa:pb:ps) | pa > pb = False
                                             | otherwise = horarioOrdenado (pb:ps)
                         medicamentoOrdenado [] = True
                         medicamentoOrdenado [_] = True
                         medicamentoOrdenado (pa:pb:ps) | fst(pa) > fst(pb) = False
                                             | otherwise = medicamentoOrdenado (pb:ps)
                                                
planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido ((h,m):ps) | (horarioRepetido h ps || medicamentoRepetido m || not (horarioOrdenado ((h,m):ps))) || not (medicamentoOrdenado m) = False
                             | otherwise = planoValido ps
                         where
                         horarioRepetido h [] = False
                         horarioRepetido h (l:ls) | h == fst(l) = True
                                                      | otherwise = horarioRepetido h ls
                         repeticaoNaLista h [] = False
                         repeticaoNaLista h (l:ls) | (h == l) = True
                                            | otherwise = repeticaoNaLista h ls
                         medicamentoRepetido [] = False
                         medicamentoRepetido (m:ms) | repeticaoNaLista m ms = True
                                                | otherwise = medicamentoRepetido ms
                         horarioOrdenado [] = True
                         horarioOrdenado [_] = True
                         horarioOrdenado (pa:pb:ps) | fst(pa) > fst(pb) = False
                                             | otherwise = horarioOrdenado (pb:ps)
                         medicamentoOrdenado [] = True
                         medicamentoOrdenado [_] = True
                         medicamentoOrdenado (pa:pb:ps) | pa > pb = False
                                             | otherwise = medicamentoOrdenado (pb:ps)

main :: IO ()
main =  do
print(receituarioValido [("b", [3,2,1]), ("c", [3,2,1]), ("f", [1])])
print(receituarioValido [("batata", [1,2,3]), ("cenoura", [1,2,3]), ("feijao", [])])
print(receituarioValido [("cenoura", [3,2,1]), ("batata", [3,2,1]), ("feijao", [])])
print(receituarioValido [("cenoura", [3,2,1]), ("batata", [3,2,2,1]), ("batata", [])])
print(receituarioValido [("cenoura", [3,2,1]), ("batata", [3,2,2,1]), ("feijao", [])])
print(planoValido [(1,["b","c","d"]), (2,["b","c","d"])])