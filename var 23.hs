import Lab2
students' = students'' students []
students'' (x:xs) a = proverka (name x) (surname x) (year x) (discrete_mathematics) a xs
students'' [] a = a
proverka q w e ((y,d):ys) a xs = if ((q == y) && (w == d)) then (students'' xs (((q,w),(2013 - e)):a)) else (proverka q w e ys a xs)
proverka q w e [] a xs = students'' xs a

main = do
putStrLn "Возраст, фамилия и имя студента:"
mapM_ putStrLn $ map (\((name,surname),pos) -> show pos ++ " " ++ surname ++ " " ++ name) (reverse students')