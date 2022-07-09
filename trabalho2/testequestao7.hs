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

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario [] = []
geraPlanoReceituario (r:rs) = geraHorarios (fst(r)) (snd(r)) ++ geraPlanoReceituario rs
                              where
                              geraHorarios _ [] = []
                              geraHorarios m (h:hs) = (h,[m]):(geraHorarios m hs)

main :: IO ()
main =  do
print(geraPlanoReceituario rec1)