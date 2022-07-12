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
plantaoCorreto p estoque = concatena (geraPlantao p) (listaCompras (comparaEstoque (geraEstoquePlano p []) estoque))
                            where
                            quickSort [] = []
                            quickSort ((ma,ha):rs) = quickSort [x | x <- rs, fst(x) <= ma] ++ [(ma, ha)] ++ quickSort [x | x <- rs, fst(x) > ma]
                            
                            concatena ((h,m):ps) e = ((h,m ++ e):ps)
                            
                            geraPlantao [] = []
                            geraPlantao ((h,m):ps) = (h, converteCuidado m):(geraPlantao ps)
                                                        where
                                                        converteCuidado med = [Medicar m | m <- med]
                                                        
                            listaCompras e = [Comprar m q | (m,q) <- e]
                            
                            -- previsto real
                            comparaEstoque [] _ = []
                            comparaEstoque _ [] = []
                            comparaEstoque ((ma,qa):eas) ((mb,qb):ebs) | ma == mb && qa <= qb = comparaEstoque eas ebs
                                                                       | ma == mb && qa > qb = (ma, qa - qb):(comparaEstoque eas ebs)
                                                                       | otherwise = (ma, qa):(comparaEstoque eas ((mb,qb):ebs))

                            geraEstoquePlano [] e = quickSort e
                            geraEstoquePlano ((h,m):ps) e = geraEstoquePlano ps (atualizaEstoque m e)
                                                            where
                                                            atualizaEstoque [] e = e
                                                            atualizaEstoque (m:ms) e | (consultarMedicamento m e) > 0 = atualizaEstoque ms (comprarMedicamento m 1 e)
                                                                                     | otherwise = atualizaEstoque ms ((m,1):e)
                            
                            
                            

main :: IO ()
main =  do
print(plantaoCorreto plano estoque)


