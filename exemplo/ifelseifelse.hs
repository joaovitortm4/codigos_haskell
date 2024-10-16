classificaNota :: Int -> String
classificaNota nota =
    if nota >= 9
        then "SS"
        else if nota >= 7
             then "MS"
             else if nota >= 5
                  then "MM"
                  else if nota >= 3
                       then "MI"
                       else if nota > 0
                            then "II"
                            else "SR"

main :: IO ()
main = do
    putStrLn "Digite a nota: "
    input <- getLine
    let nota = read input :: Int
    putStrLn ("Menção: " ++ classificaNota nota)
