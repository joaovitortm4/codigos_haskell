import qualified Data.Map as Map -- importa estrutura de mapa para armazenar os valores da sequencia
import System.IO 

-- Função de Fibonacci com memoization
fibonacci :: Int -> Integer
fibonacci n = fst (fibMemo n Map.empty) -- cria um mapa vazio com tuplas no formato (Integer, Map.Map Int Integer)
              --  fst -> retirar o primeiro elemento de uma tupla, no caso o valor de fibonacci para 'n'
  where
    fibMemo 0 memo = (0, memo) -- quando o valor de entrada for 0, fibonacci vale 0 e o mapa é retornado sem alterações
    fibMemo 1 memo = (1, memo) 
    fibMemo x memo =
      case Map.lookup x memo of -- verificação se o valor de fibonacci para x ja foi calculado e esta na memoria
        Just val -> (val, memo)  -- se já estiver no cache, retorna o valor
        Nothing -> 
          let (fib1, memo1) = fibMemo (x - 1) memo -- calcula o valor de fibonacci para x-1 e atualiza o mapa
              (fib2, memo2) = fibMemo (x - 2) memo1
              result = fib1 + fib2
          in (result, Map.insert x result memo2)


-- Função principal
main :: IO ()
main = do
  putStrLn "Quantos números da sequência de Fibonacci você deseja calcular?"
  hFlush stdout  -- garante que a mensagem será exibida antes da entrada
  input <- getLine
  let n = read input :: Int
  let output = fibonacci n
  putStrLn $ "A sequência de Fibonacci até " ++ show n ++ " números é " ++ show output