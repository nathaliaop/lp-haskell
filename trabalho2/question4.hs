demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos (p:ps) = ordena ((fst(p), (length (snd(p)))):demandaMedicamentos ps)
                            where
                            ordena [] = []
                            ordena [a] = [a]
                            ordena (la:lb:ls) | la > lb = lb:la:ordena ls
                                            | otherwise = la:lb:ls

main :: IO ()
main =  do
print(demandaMedicamentos [("cenoura", [3,2,1]), ("batata", [3,2,2,1]), ("feijao", [])])