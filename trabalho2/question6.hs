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

type Plantao = [(Horario, [Cuidado])]

data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

instance Show Cuidado where
  show (Comprar m q) =
    "Comprar "
      ++ Prelude.show q
      ++ " comprimido(s) do medicamento: "
      ++ m
  show (Medicar m) = "Ministrar medicamento: " ++ m

plantaoValido :: Plantao -> Bool
plantaoValido p | horarioDistintoCrescente p && medicarDistintoOrdenado p = True
                | otherwise = False
                         where
                         horarioDistintoCrescente [] = True
                         horarioDistintoCrescente [_] = True
                         horarioDistintoCrescente (pa:pb:ps) | fst(pa) >= fst(pb) = False
                                                             | otherwise = horarioDistintoCrescente (pb:ps)
                         comprar (Medicar m) (Comprar c _) | m == c = True
                         comprar (Comprar m _) (Medicar c) = m == c
                         comprar _ _ = False
                         existeMedicarComprar med [] = False                    
                         existeMedicarComprar med (c:cs) | comprar med c = True
                                                      | otherwise = existeMedicarComprar c cs
                         medicarComprarIguais [] = False
                         medicarComprarIguais (c:cs) | existeMedicarComprar c cs = True
                                                     | otherwise = medicarComprarIguais cs
                         filtraMedicar c = filter isMedicar c
                            where
                            isMedicar (Medicar _) = True
                            isMedicar _ = False
                         medicarDistintoOrdenado [] = True
                         medicarDistintoOrdenado ((h, c):ps) | medicarComprarIguais c || (not (medicamentoOrdenado (filtraMedicar c))) = False
                                                             | otherwise = medicarDistintoOrdenado ps
                         medicamentoOrdenado [] = True
                         medicamentoOrdenado [_] = True
                         medicamentoOrdenado (Medicar ca:Medicar cb:cs) | ca > cb = False
                                                                        | otherwise = medicamentoOrdenado (Medicar cb:cs)

main :: IO ()
main =  do
print(plantaoValido [ (6, [Medicar med6]),
    (8, [Medicar med4]),
    (17, [Medicar med6, Medicar med4]),
    (22, [Medicar med7])
  ])