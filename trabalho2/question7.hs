med1 :: Medicamento
med1 = "Adera"

med2 :: Medicamento
med2 = "Alprazolam"

med3 :: Medicamento
med3 = "Donepezila"

med4 :: Medicamento
med4 = "Lactulona"

med5 :: Medicamento
med5 = "Mirtazapina"

med6 :: Medicamento
med6 = "Pantoprazol"

med7 :: Medicamento
med7 = "Patz"

med8 :: Medicamento
med8 = "Quetiapina"

med9 :: Medicamento
med9 = "Xarelto"

rec1 :: Receituario
rec1 = [(med4, [8, 17]), (med6, [6]), (med7, [22]), (med8, [8, 22, 23])]

type Medicamento = String

type Horario = Int

type Receituario = [(Medicamento, [Horario])]

type PlanoMedicamento = [(Horario, [Medicamento])]

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano [] = []
geraReceituarioPlano (p:ps) = quickSort (comprimeReceituario (geraReceituario (fst(p)) (snd(p)) ++ geraReceituarioPlano ps))
                                where
                                geraReceituario _ [] = []
                                geraReceituario h (m:ms) = (m,[h]):(geraReceituario h ms)
                                
                                comprimeReceituario [] = []
                                comprimeReceituario (p:ps) = (adicionaHorario p ps) ++ comprimeReceituario (filtraRepetidos (fst(p)) (ps))
                                                        where
                                                        
                                                        adicionaHorario (m,h) [] = [(m,h)]
                                                        adicionaHorario (m,h) (p:ps) | existe m p = (adicionaHorario (m, h ++ snd(p)) ps)
                                                                                     | otherwise = adicionaHorario (m,h) ps
                                                                                     where
                                                                                     existe med (m,h) = med == m
                                
                                                        filtraRepetidos m ps = filter (horariosIguais m) ps
                                                                               where horariosIguais m p = m /= fst(p)
                                quickSort [] = []
                                quickSort ((ma,ha):rs) = quickSort [x | x <- rs, fst(x) <= ma] ++ [(ma, ha)] ++ quickSort [x | x <- rs, fst(x) > ma]
                                
geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario (r:rs) = quickSort (comprimePlano (geraPlano (fst(r)) (snd(r)) ++ geraPlanoReceituario rs))
                                where
                                geraPlano _ [] = []
                                geraPlano m (h:hs) = (h,[m]):(geraPlano m hs)

                                comprimePlano [] = []
                                comprimePlano (r:rs) = (adicionaMedicamento r rs) ++ comprimePlano (filtraRepetidos (fst(r)) (rs))
                                                        where
                                                        
                                                        adicionaMedicamento (h,m) [] = [(h,m)]
                                                        adicionaMedicamento (h,m) (r:rs) | existe h r = (adicionaMedicamento (h, m ++ snd(r)) rs)
                                                                                    | otherwise = adicionaMedicamento (h,m) rs
                                                                                    where
                                                                                    existe hor (h,m) = hor == h
                                
                                                        filtraRepetidos h rs = filter (horariosIguais h) rs
                                                                               where horariosIguais h r = h /= fst(r)


                                quickSort [] = []
                                quickSort ((ha,ma):ps) = quickSort [x | x <- ps, fst(x) <= ha] ++ [(ha, ma)] ++ quickSort [x | x <- ps, fst(x) > ha]


main :: IO ()
main =  do
print(geraReceituarioPlano [(6,["Pantoprazol"]),(8,["Lactulona","Quetiapina"]),(17,["Lactulona"]),(22,["Patz","Quetiapina"]),(23,["Quetiapina"])])
print(geraPlanoReceituario [("Lactulona", [8, 17]), ("Pantoprazol", [6]), ("Patz", [22]), ("Quetiapina", [8, 22, 23])])