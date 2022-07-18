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

type PlanoMedicamento = [(Horario, [Medicamento])]

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

plano :: PlanoMedicamento
plano = [(6, [med6]), (17, [med4]),(18, [med4]), (22, [med7])]

estoque :: EstoqueMedicamentos
estoque = [ (med6, 1), (med7, 10)]

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento m (e:es) | (fst(e) == m) && snd(e) > 1 = Just ((m, snd(e) - 1):es)
                          | (fst(e) == m) && snd(e) == 1 = Just es
                          | otherwise = case (tomarMedicamento m es) of
                                                   Just n -> Just (e:n)
                                                   Nothing -> Nothing

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento m q [] = [(m,q)]
comprarMedicamento m q (e:es) | not (existe m (e:es)) = (m,q):(e:es)
                              | (fst(e) == m) = (m,q + snd(e)):es 
                              | otherwise = e:comprarMedicamento m q es
                              where
                              existe m [] = False
                              existe m (l:ls) | (fst(l) /= m) = existe m ls
                                             | (fst(l) == m) = True

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento m [] = 0
consultarMedicamento m (e:es) | (fst(e) == m) = snd(e)
                              | otherwise = consultarMedicamento m es

-- gera estoque necessario para o plano e compara com o estoque real e compra tudo no inicio
-- OU
-- tenta tomar o medicamento e se retornar Nothing, compra

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto [] _ = []
plantaoCorreto ((h,m):ps) e = (h,(geraCuidado m e)):(plantaoCorreto ps (atualizaEstoque m e))
    where
    geraCuidado [] _ = []
    geraCuidado (m:ms) e | isNothing(tomarMedicamento m e) = (Comprar m 1):(Medicar m):(geraCuidado ms e)
                         | otherwise = (Medicar m):(geraCuidado ms (fromJust(tomarMedicamento m e)))
    atualizaEstoque [] e = e
    atualizaEstoque (m:ms) e | isNothing(tomarMedicamento m e) = atualizaEstoque ms e
                              | otherwise = atualizaEstoque ms (fromJust(tomarMedicamento m e))
    isNothing Nothing = True
    isNothing _ = False
    fromJust (Just c) = c                       
                            
                            
main :: IO ()
main =  do
print(plantaoCorreto plano estoque)


