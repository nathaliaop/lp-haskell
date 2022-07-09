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

filtraMedicar c = filter isMedicar c
                            where
                            isMedicar (Medicar _) = True
                            isMedicar _ = False

comprime :: PlanoMedicamento-> PlanoMedicamento
comprime (r:rs) = addMedicamento r rs
                  where
                  addMedicamento (h,m) rs = [(h, m ++ map snd (filter mesmoHorario rs))]
                  
                  mesmoHorario 
--para cada horario ve se ja existe concaena medicamentos e filtra outros
main :: IO ()
main =  do
print(comprime [(8,["Lactulona"]),(17,["Lactulona"]),(6,["Pantoprazol"]),(22,["Patz"]),(8,["Quetiapina"]),(22,["Quetiapina"]),(23,["Quetiapina"])])