--Base64 kódolás és dekódolás.
import Data.Char

--Szótár előállítása: -> összetétele: (A-Z) + (a-z) + (0-9) + (+ /)
--Az olvashatóság érdekében bevezettem egy saját 'Dictionary' típust.
type Dictionary = [(Int, Char)]

dictionary :: Dictionary
dictionary = [(i, chr(i + 65)) | i <- [0..25]] ++ --A nagybetük kigenerálása.
             [(26 + i, chr(i + 97)) | i <- [0..25]] ++ --A kisbetük kigenerálása.
             [(52 + i, chr(i + 48)) | i <- [0..9]] ++ --A számok kigenerálása.
             [(62, '+'), (63, '/')] -- + / karakterek hozzáfüzése.

--Listák kiegészítése adott méretűre. 
pad :: ([a] -> [a] -> [a]) -> a -> Int -> [a] -> [a]
pad func v ctr ls
    | ctr <= len = ls
    | otherwise = foldl func tmpList [ls]
    where
        tmpList = take (ctr - len) (repeat v )
        len = length ls


--A fenti függvény segítségével megadjuk a jobbról és balról kiegészítő függvényeket.
padLeft :: a -> Int -> [a] -> [a]
padLeft  = pad (\p -> (p ++))

padRight :: a -> Int -> [a] -> [a]
padRight = pad (\p -> (++ p))

--Az olvashatóság érdekében bevezettem egy saját 'BitString' típust.
type BitString = [Int]

--Szám bitek sorozatává alakítása
toBitString :: Int -> BitString
toBitString nr = aux nr []
    where
    aux :: Int -> [Int] -> BitString
    aux 0 _ = []
    aux n ls
        | n < 2 = n : ls
        | otherwise = aux (div n 2) (mod n 2 : ls)  

--Bitek sorozata számmá alakítása.
fromBitString :: BitString -> Int
fromBitString ls = aux ls (length ls - 1)
    where
    aux :: BitString -> Int ->Int
    aux [] _ = 0
    aux (x:xs) n = (x * (2 ^ n)) + aux xs (n - 1)

--Szöveg bájtsorozattá alakítása.
toBinary :: String -> BitString
toBinary [] = []
toBinary (x:xs) = val ++ toBinary xs
    where 
        tmp = toBitString $ ord x
        val = padLeft 0 8 tmp

--Listák felszeletelése.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
    | n <= 0 = error "Helytelen hosszusag"
    | null ls = []
    | otherwise = firstL : chunksOf n secondL 
    where 
        (firstL, secondL) = splitAt n ls

--Elem keresése.
findFirst :: (a -> Bool) -> [a] -> a
findFirst _ [] = error "Nincs talalat"
findFirst func (x:xs)
    | func x == True = x
    | otherwise = findFirst func xs

--Keresés a szótárban.
findChar :: Dictionary -> BitString -> Char
findChar [] _ = error "Nincs talalat"
findChar dct ls 
    | idx < 0 || idx > 63 = error "Nincs talalat"
    | otherwise = snd (dct !! idx)
    where
        idx = fromBitString ls

--Szöveg fordítása.
translate :: Dictionary -> String -> String
translate dcty str = aux dcty lsChr
    where
        tmp = toBinary str
        len = length tmp
        padNr = ceiling (fromIntegral len / 6)
        ls = padRight 0 (6 * padNr) tmp
        lsChr = chunksOf 6 ls

        --Segedfuggveny a bitsorozatokoz tartozo karakterek felepitesehez.
        aux :: Dictionary -> [BitString] -> String
        aux _ [] = []
        aux dct (x:xs) = (findChar dct x) : aux dct xs

--Szabványos kód előállítása.
encode :: String -> String
encode str = padRight '=' (4 * padNr) tmp
    where
        tmp = translate dictionary str
        len = length tmp
        padNr = ceiling (fromIntegral len / 4)


--Keresés a szótárban2.
findCode :: Dictionary -> Char -> BitString
findCode [] _ = error "Nincs talalat"
findCode (x:xs) ch
    | snd x == ch = toBitString (fst x)
    | otherwise = findCode xs ch

--Szöveg visszafejtése.
decode :: String -> String
decode str = aux3 dictionary $ aux2 $ aux1 dictionary str
    where
        aux1 :: Dictionary -> String -> BitString
        aux1 [] _ = error "Ures lista"
        aux1 _ [] = []
        aux1 dcty1 (x:xs)
            | x == '=' = []
            | otherwise = tmp ++ aux1 dcty1 xs
            where
                tmp = padLeft 0 6 (findCode dcty1 x)

        aux2 :: BitString -> [BitString]
        aux2 ls = chunksOf 8 ls

        aux3 :: Dictionary -> [BitString] -> String
        aux3 [] _ = error "Ures szotar"
        aux3 _ [] = []
        aux3 dcty2 (x:xs)
            | length x == 8 =  tmp : aux3 dcty2 xs
            | otherwise = aux3 dcty2 xs
            where
                tmp = chr $ fromBitString x

main :: IO ()
main = do  
    str <- readFile "test.txt"

    putStrLn "Encoded text:"
    print $ encode str

    putStrLn "\nDecoded text:"
    print $ decode $ encode str

    