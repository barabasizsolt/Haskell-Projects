--Lempel-Ziv-Welch adattomorites

import Data.Char (chr, ord)
import Data.List ( (\\), isPrefixOf )

--Az alapszotar elkeszitese.(A szotar az algoritmus eseteben tetszoleges)
dictionary :: [String]
dictionary = [ [chr i ] | i <- [0..127] ]
--dictionary = "." : " " : [ [i] | i <- ['A'..'Z'] ]

--Az illeszkedo kezdoszelet megkeresese.
prefixes :: String -> [String] -> [(Int,String)]
prefixes str dcty = aux str dcty 0
    where
        aux :: String -> [String] -> Int -> [(Int,String)]
        aux _ [] _ = []
        aux str1 (x:xs) idx
            | x `isPrefixOf` str1 = (idx, x) : aux str1 xs (idx + 1)
            | otherwise = aux str1 xs (idx + 1)

--A leghosszabb illeszkedo kezdoszelet kivalasztasa.
longest :: [(Int,String)] -> (Int,String)
longest [] = error "Ures lista"
longest [x] = x
longest (x:xs)
    | length (snd x) > length (snd (longest xs)) = x
    | otherwise = longest xs

--A tomoritendo szoveg inkrementalis feldolgozasa.
munch :: [String] -> String -> (Int,String,String)
munch dcty str = (x, y, z) 
    where
        (x, y) = longest $ prefixes str dcty
        z = str \\ y

--A szotar bovitese.
append :: [String] -> String -> String -> [String]
append dcty str rem
    | null rem = dcty
    | not (aux tmp dcty) = dcty ++ [tmp]
    | otherwise = dcty
        where
            aux :: String -> [String] -> Bool
            aux _ [] = False
            aux str1 (x:xs)
                | str1 == x = True
                | otherwise = aux str1 xs

            tmp = str ++ [head rem]

--Bekodolas.
encode :: [String] -> String -> [Int]
encode dcty str
    | null z = [x]
    | len > 256 = x : encode dcty z
    | otherwise = x : encode newDcty z
    where
        (x, y, z) = munch dcty str
        newDcty = append dcty y z
        len = length dcty

--Betomorites.
compress :: String -> String
compress str = aux tmp
    where
        aux :: [Int] -> String
        aux [] = []
        aux (x:xs) = chr x : aux xs

        tmp = encode dictionary str

--Kikodolas.
decode :: [String] -> [Int] -> String
decode dcty [x] = dcty !! x
decode dcty (x:y:xs)
    | y >= length dcty = chx ++ decode newDcty2 (y:xs) --nagyobb, nem kisebb
    | otherwise = chx ++ decode newDcty1 (y:xs)
        where
            chx = dcty !! x
            chy = dcty !! y
            newDcty1 = dcty ++ [chx ++ chy]
            newDcty2 = dcty ++ [chx ++ [head chx]]

--Kitomorites.
decompress :: String -> String
decompress str = decode dictionary (aux str)
    where
        aux :: String -> [Int]
        aux [] = []
        aux (x:xs) = ord x : aux xs

--Test
--decompress . compress $ "lalalala"
--decompress . compress $ "aaabbbccc"
--encode dictionary "ABABCBABABAAAAAAAABAAA"  --> Kasa adatszerk pelda