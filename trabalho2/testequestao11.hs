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
plano = [(6, [med6]), (8, [med4]), (17, [med4]), (22, [med7])]

estoque :: EstoqueMedicamentos
estoque = [(med4, 10), (med6, 5), (med7, 10)]

-- estoquePlano :: PlanoMedicamento -> EstoqueMedicamentos
-- estoquePlano [] est = est
-- estoquePlano ((h,m):ps) est = estoquePlano ps (atualizaEstoque m est )

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

atualizaEstoque :: [Medicamento] -> EstoqueMedicamentos -> EstoqueMedicamentos
atualizaEstoque [] e = e
atualizaEstoque (m:ms) e | (consultarMedicamento m e) > 0 = atualizaEstoque ms (comprarMedicamento m 1 e)
                         | otherwise = atualizaEstoque ms ((m,1):e)

geraEstoquePlano :: PlanoMedicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
geraEstoquePlano [] e = e
geraEstoquePlano ((h,m):ps) e = geraEstoquePlano ps (atualizaEstoque m e)
          
plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto = undefined

-- gera estoque necessario para o plano e compaara com o estoque real e compra tudo no inicio
-- OU
-- tenta tomar o medicamento e se retornar Nothing, compra

main :: IO ()
main =  do
print(geraEstoquePlano plano [])
print(atualizaEstoque [med4] estoque)