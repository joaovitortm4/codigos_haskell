main :: IO()
main = do
    putStr "Digite seu nome: "
    nome <- getLine
    putStr ("Olá, " ++ nome ++ "!")
