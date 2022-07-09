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

final :: Receituario -> PlanoMedicamento
final [] = []
final (r:rs) = (integra (geraPlanoReceituario (r:rs)))

integra :: PlanoMedicamento -> PlanoMedicamento
integra [] = []
integra (r:rs) = (addMedicamento r rs) ++ (integra rs)

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario (r:rs) = geraHorarios (fst(r)) (snd(r)) ++ geraPlanoReceituario rs
                              where
                              geraHorarios _ [] = []
                              geraHorarios m (h:hs) = (h,[m]):(geraHorarios m hs)
                              
addMedicamento :: (Horario, [Medicamento]) -> [(Horario, [Medicamento])]-> [(Horario, [Medicamento])]
addMedicamento _ [] = []
addMedicamento (h,m) (r:rs) | existe h r = (h, m ++ snd(r)):(addMedicamento (h,m) rs)
                            | otherwise = addMedicamento (h,m) rs
                            where
                            existe hor (h,m) = hor == h
                            
filtraRepetidos :: Horario -> [(Horario, [Medicamento])]-> [(Horario, [Medicamento])]
filtraRepetidos h rs = filter (funcao h) rs
                       where funcao h r = h /= fst(r)

--para cada horario ve se ja existe concatena medicamentos e filtra outros
main :: IO ()
main =  do
print(geraPlanoReceituario [("Lactulona", [8, 17]), ("Pantoprazol", [6]), ("Patz", [22]), ("Quetiapina", [8, 22, 23])])
print(final rec1)
print(integra [(8,["Lactulona"]),(17,["Lactulona"]),(6,["Pantoprazol"]),(22,["Patz"]),(8,["Quetiapina"]),(22,["Quetiapina"]),(23,["Quetiapina"])])
print(geraPlanoReceituario rec1)
print(filtraRepetidos 8 [(17,["Lactulona"]),(6,["Pantoprazol"]),(22,["Patz"]),(8,["Quetiapina"]),(22,["Quetiapina"]),(23,["Quetiapina"])])
print(addMedicamento (8,["Lactulona"]) [(17,["Lactulona"]),(6,["Pantoprazol"]),(22,["Patz"]),(8,["Quetiapina"]),(22,["Quetiapina"]),(23,["Quetiapina"])])