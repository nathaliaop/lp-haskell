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

type Medicamento = String

type Quantidade = Int

type Horario = Int

type EstoqueMedicamentos = [(Medicamento, Quantidade)]

type Plantao = [(Horario, [Cuidado])]

data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

instance Show Cuidado where
  show (Comprar m q) =
    "Comprar "
      ++ Prelude.show q
      ++ " comprimido(s) do medicamento: "
      ++ m
  show (Medicar m) = "Ministrar medicamento: " ++ m

plantao :: Plantao
plantao =
  [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med4, Comprar med7 30]),
    (22, [Medicar med7])
  ]
  
estoque :: EstoqueMedicamentos
estoque = [(med4, 30), (med6, 1), (med7, 22)]
  
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q [] = [(m,q)]
comprarMedicamento m q (e:es) | not (existe m (e:es)) = (m,q):(e:es)
                              | (fst(e) == m) = (m,q + snd(e)):es 
                              | otherwise = e:comprarMedicamento m q es
                              where
                              existe m [] = False
                              existe m (l:ls) | (fst(l) /= m) = existe m ls
                                              | (fst(l) == m) = True
                                             
tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento m (e:es) | (fst(e) == m) && snd(e) > 1 = Just ((m, snd(e) - 1):es)
                          | (fst(e) == m) && snd(e) == 1 = Just es
                          | otherwise = case (tomarMedicamento m es) of
                                        Just n -> Just (e:n)
                                        Nothing -> Nothing
                                        
executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] est = Just est
executaPlantao ((h,c):hs) est | isNothing (executaCuidado c est) = Nothing
                              | otherwise = executaPlantao hs (fromJust (executaCuidado c est))
                                where
                                fromJust (Just c) = c
                                isNothing (Nothing) = True
                                isNothing _ = False
                                executaCuidado [] est = Just est
                                executaCuidado (c:cs) est | isNothing (cuida c) = Nothing
                                                          | otherwise = executaCuidado cs (fromJust (cuida c))
                                cuida (Comprar m q) = Just (comprarMedicamento m q est)
                                cuida (Medicar m) = tomarMedicamento m est

main :: IO ()
main =  do
print(executaPlantao plantao estoque)