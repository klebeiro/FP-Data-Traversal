import Data.List

data Biblioteca = Livro String String [String] Int String
    |TCC String String String Int String
    |Artigo String String [String] Int String
        deriving (Eq,Ord,Show)

item1,item2,item3 :: Biblioteca
item1 = Livro "1" "Programacao Funcional" ["Silvio Meira"] 1980 "livro"
item2 = TCC "1234" "Trabalho de graduacao" "Maria Antonio" 2020 "tcc"
item3 = Artigo "4567" "Aprendendo Haskell" ["Autor1","Autor2"] 2001 "artigo"

lista :: [Biblioteca]
lista = [item1,item2,item3]

-- Questão 1
listarTudo :: [Biblioteca] -> IO()
listarTudo = mapM_ mostrarItem

-- Questão 2
listarLivros :: [Biblioteca] -> IO()
listarLivros lista = listarTudo(filter (\n -> isLivro n) lista)

-- Questão 3
listarTCCs :: [Biblioteca] -> IO()
listarTCCs lista = listarTudo(filter (\n -> isTCC n) lista)

-- Questão 4
listarArtigos :: [Biblioteca] -> IO()
listarArtigos lista = listarTudo(filter (\n -> isArtigo n) lista)

-- Questão 5
listarPorAno :: [Biblioteca] -> Int -> IO()
listarPorAno lista ano = listarTudo(filter (\n -> isAnoEqual n ano) lista)

-- Questão 6
listarPorEstante :: [Biblioteca] -> String -> IO()
listarPorEstante lista estante = listarTudo(filter (\n -> isLivro n && isEstanteEqual n estante) lista)

{-

- Outra forma resolver a questão 1 usando recursão:

listarTudo :: [Biblioteca] -> IO()
listarTudo [] = return ()
listarTudo (x:xs) = do mostrarItem x;
                        listarTudo xs;

- Outras formas de resolver as questões 2 à 6:

Usando list comprehension:
listarTipo lista = listarTudo [x | x <- lista, isTipo x]

Usando recursão:
listarTipo [] = return ()
listarTipo (x:xs)
    | isTipo x = do mostrarItem x; listarTipo xs;
    | otherwise = listarTipo xs;
-}

-- Funções utilitárias

isLivro :: Biblioteca -> Bool
isLivro (Livro estante titulo listaAutores ano cat) = True;
isLivro _ = False

isTCC :: Biblioteca -> Bool
isTCC (TCC cod titulo autor ano cat) = True;
isTCC _ = False

isArtigo :: Biblioteca -> Bool
isArtigo (Artigo cod titulo listaAutores ano cat) = True;
isArtigo _ = False

isAnoEqual :: Biblioteca -> Int -> Bool
isAnoEqual (Livro estante titulo listaAutores ano cat) x = ano == x
isAnoEqual (TCC cod titulo autor ano cat) x = ano == x
isAnoEqual (Artigo cod titulo listaAutores ano cat) x = ano == x

isEstanteEqual :: Biblioteca -> String -> Bool
isEstanteEqual (Livro estante titulo listaAutores ano cat) x = estante == x

mostrarItem :: Biblioteca -> IO()
mostrarItem (Livro estante titulo listaAutores ano cat) = do
    putStrLn "------LIVRO------"
    putStrLn ("Estante: " ++ show estante)
    putStrLn ("Titulo: " ++ show titulo)
    putStrLn ("Autor(es): " ++ intercalate ", " listaAutores)
    putStrLn ("Ano: " ++ show ano)
    putStrLn ("Categoria: " ++ show cat)

mostrarItem (TCC cod titulo autor ano cat) = do
    putStrLn "------TCC------"
    putStrLn ("Código: " ++ show cod)
    putStrLn ("Titulo: " ++ show titulo)
    putStrLn ("Autor: " ++ show autor)
    putStrLn ("Ano: " ++ show ano)
    putStrLn ("Categoria: " ++ show cat)

mostrarItem (Artigo cod titulo listaAutores ano cat) = do
    putStrLn "------Artigo------"
    putStrLn ("Código: " ++ show cod)
    putStrLn ("Titulo: " ++ show titulo)
    putStrLn ("Autor(es): " ++ intercalate ", " listaAutores)
    putStrLn ("Ano: " ++ show ano)
    putStrLn ("Categoria: " ++ show cat)