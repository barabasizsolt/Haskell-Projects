module Base64 (encode64, decode64) where

--Base64 kódolás és dekódolás.

--Az algoritmus ismertetése (Base64 formátum előállítása):
--                            "Man"
--                            / | \
--                          77 97 110
--                          /  |     \
--                        /    |      \
--                      /      |       \
--              01001101    01100001   01101110
--                    \       |          /
--                  010011010110000101101110
--                   /      /     \       \
--               010011   010110  00101  101110
--                  \       \       /       /
--                  "T"     "W"    "F"     "u"

-- "Man" -> "TWFu"

import Data.Char

--Szótár előállítása: -> összetétele: (A-Z) + (a-z) + (0-9) + (+ /)
--Az olvashatóság érdekében bevezettem egy saját 'Dictionary' típust.
type Dictionary = [(Int, Char)]

dictionary :: Dictionary
dictionary = zip [0..] (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/")

--Listák kiegészítése adott méretűre. 
--paraméterek: [1] -> függvény, milyen módon (jobbról, balról, stb.) szeretnék a listát kiegészíteni.
--             [2] -> az elem amivel kiegészítjük.
--             [3] -> szám, amekkorára ki szeretnénk egészíteni, ha a lista hosszabb mint a megadott szám, akkor nem módosítja.
-- -> pad (\p l -> (p ++ l)) 'a' 7 "bbb" -> "aaaabbb"
pad :: ([a] -> [a] -> [a]) -> a -> Int -> [a] -> [a]
pad func v ctr ls
    | ctr <= len = ls
    | otherwise = foldl func tmpList [ls]
    where
        tmpList = replicate (ctr - len) v
        len = length ls


--A fenti függvény segítségével megadjuk a jobbról és balról kiegészítő függvényeket.
--balról
padLeft :: a -> Int -> [a] -> [a]
padLeft  = pad (\p -> (p ++))
--jobbról
padRight :: a -> Int -> [a] -> [a]
padRight = pad (\p -> (++ p))

--Az olvashatóság érdekében bevezettem egy saját 'BitString' típust.
type BitString = [Int]

--Szám bitek sorozatává alakítása.
-- -> bitek sorozatává alakít egy nem negatív számot.
-- -> toBitString 10 -> [1,0,1,0]
toBitString :: Int -> BitString
toBitString nr = aux nr []
    where
    aux :: Int -> [Int] -> BitString
    aux 0 _ = []
    aux n ls
        | n < 2 = n : ls
        | otherwise = aux (div n 2) (mod n 2 : ls)  

--Bitek sorozata számmá alakítása.
-- -> a megadott bitek sorozatából felépití a számot.
-- -> fromBitString [1,0,1,0,0] -> 20
fromBitString :: BitString -> Int
fromBitString ls = aux ls (length ls - 1)
    where
    aux :: BitString -> Int ->Int
    aux [] _ = 0
    aux (x:xs) n = (x * (2 ^ n)) + aux xs (n - 1)

--Szöveg bájtsorozattá alakítása.
-- -> a szöveg elemeit a megfelelő kódra kell alakítani az 'ord' függvény, majd összefűzni.
-- -  !ha rövidebb kódot kapunk mint 8, ki kell egészíteni balról 0-val.
-- -> toBinary "SOS" -> [0,1,0,1,0,0,1,1,0,1,0,0,1,1,1,1,0,1,0,1,0,0,1,1]
toBinary :: String -> BitString
toBinary [] = []
toBinary (x:xs) = val ++ toBinary xs
    where 
        tmp = toBitString $ ord x -- a karakterek egyenkénti kóddá alakítása. (ascii -> bitstring)
        val = padLeft 0 8 tmp

--Listák felszeletelése.
-- -> a megadott listát a kért hosszúságuakra darabolja.
-- -> chunksOf 6 [1..10] -> [[1, 2, 3, 4, 5, 6], [7, 8, 9, 10]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
    | n <= 0 = error "Helytelen hosszúság"
    | null ls = []
    | otherwise = firstL : chunksOf n secondL 
    where 
        (firstL, secondL) = splitAt n ls

--Elem keresése.
-- -> a megadott tulajdonságú elemet megkeresi a listában.
-- -> findFirst (<4) [1..10] -> 1
findFirst :: (a -> Bool) -> [a] -> a
findFirst _ [] = error "Nincs találat"
findFirst func (x:xs)
    | func x = x
    | otherwise = findFirst func xs

--Keresés a szótárban.
-- -> megadja a számnak megfelelő kódót a szótárban.
-- -> findChar dictionary [1,1,1,1,0,0] -> 8
findChar :: Dictionary -> BitString -> Char
findChar [] _ = error "Nincs találat"
findChar dct ls 
    | idx < 0 || idx > 63 = error "Nincs találat"
    | otherwise = snd (dct !! idx)
    where
        idx = fromBitString ls

--Szöveg fordítása.
-- -> a megadott szövegből előállítja a Base64 formátumú ábrázolást.
-- A kód előállításának menete:
--      -> karakter kód meghatározása, bájtokká alakítása. [1]
--      -> bájtok összefűzése bitsorozattá. [2]
--      -> ha a bitsorozat nem osztható 6-al fel kell tölteni jobbról '0' -val. [2]
--      -> darabolás 6 hosszúságura. [3]
--      -> a szótár dictionary felhasználásával a bitsorozatokhoz tartozó karakterek meghatározása. [4]
--      -> szöveg előállítása. [5]

-- -> translate dictionary "Almafa" -> "QWxtYWZh"
translate :: Dictionary -> String -> String
translate dcty str = aux dcty lsChr --[4], [5]
    where
        tmp = toBinary str --[1], [2]
        len = length tmp
        padNr = ceiling (fromIntegral len / 6)
        ls = padRight 0 (6 * padNr) tmp -- [2]
        lsChr = chunksOf 6 ls -- [3]

        --Segédfüggvény a bitsorozatokoz tartozó karakterek felépítéséhez.
        aux :: Dictionary -> [BitString] -> String
        aux _ [] = []
        aux dct (x:xs) = findChar dct x : aux dct xs

--Szabványos kód előállítása.
-- -> a 'translate' fügvénnyel előállított kódsor akkor szabványos, ha osztható 4-el.
-- -> kulönben ki kell bővíteni jobbról '=' -vel.

-- -> encode64 "Save our soul" -> "U2F2ZSBvdXIgc291bA=="
encode64 :: String -> String
encode64 str = padRight '=' (4 * padNr) tmp
    where
        tmp = translate dictionary str
        len = length tmp
        padNr = ceiling (fromIntegral len / 4)

--Keresés a szótárban2.
-- ->  megadja az adott karakterhez tartozó szótárbeli indexet bináris formában.
-- -> findCode dictionary 'a' -> [1,1,0,1,0]
findCode :: Dictionary -> Char -> BitString
findCode [] _ = error "Nincs találat"
findCode (x:xs) ch
    | snd x == ch = toBitString (fst x)
    | otherwise = findCode xs ch

--Szöveg visszafejtése.
-- -> Fontos: '=' -re nincs szükség!.
-- -> A 'translate' függvény esetében levő algoritmus fordított sorrendben.
-- -> BitString előállítása: dekodólt szöveg karakterei -> ASCII -> BitString (kiegészítés balról 6 hosszúra) [1]
-- -> Az előzőleg kapott BitStringek összefűzése [2]
-- -> Az összefűzött BitString darabolása 8 hosszú BitStringekre [3]
-- -> A feldarabolt BitStringek visszalakítása számra, majd a neki megfelelő karakter kódra(kódtábla szerint) [4]
-- -> A kapott karakterkódok összefűzése [5]
decode64 :: String -> String
decode64 str = aux3 dictionary $ aux2 $ aux1 dictionary str
    where
        -- [1], [2]
        aux1 :: Dictionary -> String -> BitString
        aux1 [] _ = error "Üres lista"
        aux1 _ [] = []
        aux1 dcty1 (x:xs)
            | x == '=' = []
            | otherwise = tmp ++ aux1 dcty1 xs
            where
                tmp = padLeft 0 6 (findCode dcty1 x)

        --[3]
        aux2 :: BitString -> [BitString]
        aux2 ls = chunksOf 8 ls

        -- [4], [5]
        aux3 :: Dictionary -> [BitString] -> String
        aux3 [] _ = error "Üres szótár"
        aux3 _ [] = []
        aux3 dcty2 (x:xs)
            | length x == 8 =  tmp : aux3 dcty2 xs
            | otherwise = aux3 dcty2 xs
            where
                tmp = chr $ fromBitString x