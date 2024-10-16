import System.Random (randomRIO)

-- Status do Profundo
data Status = AmeacadoPeloCapitaoPatria | Exilado | Rebaixado | MembroDosSete deriving (Eq)

-- Como os status serão printados
instance Show Status where
    show AmeacadoPeloCapitaoPatria = "Ameaçado pelo Capitão Pátria"
    show Exilado = "Exilado"
    show Rebaixado = "Rebaixado"
    show MembroDosSete = "Membro dos Sete"

-- Ações e impactos na popularidade
acoes :: [(String, Int)]
acoes = [ ("Pegou uma Funcionária da Vought", -30)
        , ("Mostrou a peça na primeira conversa com alguém", -40)
        , ("Abusou da Starlight", -50)
        , ("Falhou no resgate de um animal marinho", -25)
        , ("Ficou chapado no bar", -20)
        , ("Jogou água em crianças", -35)
        , ("Atacou um ativista dos direitos dos animais", -30)
        , ("Teve relações com o Polvo", 10)
        , ("Escreveu um livro", 15)
        , ("Se casou", 20)
        , ("Salvou um golfinho", 30)
        , ("Ajuda a limpar o oceano de plásticos", 25)
        , ("Faz um discurso ambiental emocionante", 20)
        ]

-- Randomizador
escolherAcao :: IO (String, Int)
escolherAcao = do
    indice <- randomRIO (0, length acoes - 1)
    return (acoes !! indice)

-- Determinar status
determinarStatus :: Int -> Status
determinarStatus p
    | p >= 75 = MembroDosSete
    | p >= 50 = Rebaixado
    | p >= 20 = Exilado
    | otherwise = AmeacadoPeloCapitaoPatria
    
-- Loop de execução
loop :: Int -> IO ()
loop popularidade = do

    putStrLn "Digite qualquer tecla para executar uma ação (ou 'q' para sair):"
    comando <- getLine
    if comando == "q"
        then putStrLn "Saindo do programa..."
        else do
            (acao, impacto) <- escolherAcao
            let novaPopularidade = max 0 (popularidade + impacto) -- Popularidade não fica negativa
            if novaPopularidade == 0
                then do
                    putStrLn $ "Ação realizada: " ++ acao ++ " (Impacto: " ++ show impacto ++ ")"
                    putStrLn "Popularidade zerada. O Capitão Pátria não tolera fracassos... O Profundo foi assassinado!"
                    return () -- Break
                else if novaPopularidade >= 100 && impacto > 0
                    then do
                        putStrLn $ "Ação realizada: " ++ acao ++ " (Impacto: " ++ show impacto ++ ")"
                        putStrLn "Parabéns! Você se tornou o herói mais famoso dos 7. Capitão Pátria te chamou para a sala de reunião e... te matou. Afinal, ele deve ser o número 1..."
                        return () -- Break
                    else do
                        putStrLn $ "Ação realizada: " ++ acao ++ " (Impacto: " ++ show impacto ++ ")"
                        putStrLn $ "Nova popularidade: " ++ show novaPopularidade
                        putStrLn $ "Status atual: " ++ show (determinarStatus novaPopularidade)
                        loop novaPopularidade

-- Função principal
main :: IO ()
main = do
    let popularidadeInicial = 75
    putStrLn "Bem-vindo ao Medidor de Popularidade do Profundo!"
    putStrLn $ "Popularidade atual: " ++ show popularidadeInicial
    loop popularidadeInicial
