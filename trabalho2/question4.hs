demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos (p:ps) = sort ((fst(p), (length (snd(p)))):demandaMedicamentos ps)
                            where
                            sort [] = []
                            sort [a] = [a]
                            sort (la:lb:ls) | la > lb = lb:la:sort ls
                                            | otherwise = la:lb:ls

main :: IO ()
main =  do
print(demandaMedicamentos [("cenoura", [3,2,1]), ("batata", [3,2,2,1]), ("feijao", [])])