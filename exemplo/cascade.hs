classificaNota :: Int -> String
classificaNota nota = case () of
    _ | nota >= 9 -> "SS"
    _ | nota >= 7 -> "MS"
    _ | nota >= 5 -> "MM"
    _ | nota >= 3 -> "MI"
    _ | nota == 0 -> "II"
    _             -> "SR"


main :: IO ()
main = do
    putStrLn "Digite a nota: "
    input <- getLine
    let nota = read input :: Int
    putStrLn ("Menção: " ++ classificaNota nota)
