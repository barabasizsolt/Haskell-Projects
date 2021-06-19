import Huffman ( decode, encode )
import Base64 ( decode64, encode64 )

--Start: main

--Főfüggvény.
main :: IO ()
main = do
    putStrLn "\n\n<---------- Kódoló / Dekódoló alkalmazás ---------->\n"

    putStrLn "Honnan szeretne olvasni?: -> Terminál[1] - Állomány[2]"
    tp <- getLine
    str <- readType tp

    putStrLn "\nVálasszon kódólási fajtát: -> Huffman[1] - Base64[2] - Mindkettő[3]"
    code <- getLine
    res <- codeType code str
    putStrLn res

    putStrLn "\nEl szeretné menteni állományba az eredményt?: -> Igen[1]"
    sv <- getLine
    saveType sv res

--OLvasás terminalról.
readFromConsole :: IO String
readFromConsole  = do
    putStrLn "Olvasson be egy karakterláncot: "
    getLine

--Olvasás állománzból.
readFromFile :: IO String
readFromFile = do
    putStrLn "Olvassa be az állomány nevét: "
    fname <- getLine
    readFile fname

--A felhasználó 'adat bekérésének' lekezelése.
readType :: [Char] -> IO String
readType x = do
    case x of
            "1" -> readFromConsole
            "2" -> readFromFile
            _ -> error "Kérem a lehetséges opciók közül válasszon!"

--Irás állományba.
saveToFile :: String -> IO ()
saveToFile str = do
    putStrLn "Olvassa be az állomány nevét: "
    fname <- getLine
    writeFile fname str
    putStrLn $ "Sikeres mentés " ++ fname ++ " néven."
    putStrLn "Kódolás vége."

--A felhasználó 'adat elmentésének' lekezelése.
saveType :: [Char] -> String -> IO ()
saveType x str = case x of
        "1" -> saveToFile str
        _ -> putStrLn "Kódolás vége."

--A felhasználó álltal választott kódolás lekezelése.

codeType :: Monad m => [Char] -> [Char] -> m [Char]
codeType x str = case x of
             "1" -> huffman str
             "2" -> base64 str
             "3" -> both str
             _ -> error "Kérem a lehetséges opciók közül válasszon!"

--Huffman kódolás és dekódolás.
huffman :: Monad m => [Char] -> m [Char]
huffman str = do
    let (code, table) = encode str
    return ("\nEncoded Huffman text:  " ++ code ++ "\nDecoded Huffman text: " ++ decode code table)


--Base64 kódolás és dekódolás.
base64 :: Monad m => String -> m [Char]
base64 str = do
    return ("\nEncoded Base64 text:   " ++ encode64 str ++ "\nDecoded Base64 text:  " ++ decode64 (encode64 str))

----Base64 - Huffman kódolás és dekódolás.
both :: Monad m => [Char] -> m [Char]
both str = do
    let codeB = encode64 str
    let (codeH, table) = encode codeB
    return ("\nEncoded Base64 - Huffman text:   " ++ codeH ++ "\nDecoded Base64 - Huffman text:   " ++ decode64 (decode codeH table))