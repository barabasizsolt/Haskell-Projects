module Huffman (encode, decode) where

--Huffman kódolás és dekódolás.

--Az algoritmus ismertetése:

--A kódolás során egy bináris fát építünk fel a következő módon:
--      -> Kiválasztjuk a két legkisebb gyakorisággal rendelkző levelet és hozzá rendelünk egy közös őst
--         amelynek a gyakorisága a két levél gyakoriságának az összege. [1]
--      -> A két levelet eltávolítjuk majd a közös őst beszúrjuk figyelve a rendezettség megőrzésére. [2]
--      -> Ismételjük az [1] folyamattól egészen addig amíg a listában van elem.

--Az így kapott fában címkézzük meg a gyökértől kiindulva a pontokat, úgy, hogy a
--bal oldalon levők 0, míg a jobb oldalon levők 1 értéket kapnak.

--A gyökértől egy adott levélig egyetlen út halad. Ezen út éleihez rendelt 0 és 1 címkéket sorrendben összeolvasva, 
--megkapjuk a levélhez rendelt karakter kódját.

-- -> a gyakoribb karakterek kódja rövidebb, a kevésbé gyakoribbaké hosszabb.

--Példa: 'almafa'
-- -> gyakoriság: [{f:1},{l:1},{m:1},{a:3}]
-- -> 1. lépés: 
--                {f:1} {l:1} {m:1} {a:3}
--                    \ /
--                   {fl:2}

-- -> 2. lépés: 
--                {f:1} {l:1} {a:3}
--                    \ /
--                    {fl:2} {m:1}
--                        \ /
--                       {mfl:3}

-- -> 3. lépés: 
--                {f:1} {l:1}
--                    0\ /1
--                   {fl:2} {m:1}
--                         1\ /0
--                         {mfl:3} {a:3}
--                             0\ /1
--                            {amfl:6}

-- -> [{a:1}, {m:00}, {f:010}, {l:011}] -> "10110010101"


import Data.List ( (\\), group, isPrefixOf, sort, sortOn)
import Data.Ord
import System.IO
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

type Frequency = Int
type LetterSum = (String, Frequency)

--Betűgyakoriság meghatározása.
-- "almafa" -> [("a",3),("f",1),("l",1),("m",1)]
countLetters :: String -> [LetterSum]
countLetters str = map (\x -> ([head x], length x)) $ group $ sort str

--Kezdőállapot meghatározása.
--A Huffman fa ábrázolása az alábbi típusok segítségével.
type Id    = Int
type Node  = (Id, LetterSum) --csomó
type Label = String --címke
type Edge  = (Id, Id, Label) --él

--Az algoritmus kezdőállapota.
--balról jobbra: -> a feldolgozandó csúcsok listája, sorrend: gyakoriság alapján növekvő.
--               -> az éllista, mely kezdetben üres.
--               -> a már feldolgozott csúcsok listája, mely kezdetben szintén üres.
type State = ([Node], [Edge], [Node])

--rendezi betűgyakoriság alapján, ellátja ID-val.
--[("a",3),("f",1),("l",1),("m",1)] -> ([(1,("f",1)),(2,("l",1)),(3,("m",1)),(4,("a",3))],[],[])
startState :: [LetterSum] -> State
startState ltSum = (aux tmpLs 1, [], [])
    where
        --ID-k beállítása.
        aux :: [LetterSum] -> Int -> [Node]
        aux [] _ = []
        aux (x:xs) idx = (idx, x) : aux xs (idx+1)

        --rendezés.
        tmpLs = sortOn snd ltSum

--Követekező azonosító előállítása.
-- -> ([(1,("f",1)),(2,("l",1)),(3,("m",1)),(4,("a",3))],[],[]) -> 5
nextId :: State -> Id
nextId (ls, _, _) = aux ls + 1
    where
        aux :: [Node] -> Id
        aux [x] = fst x
        aux (x:xs)
            | fst x > aux xs = fst x
            | otherwise = aux xs

--Új csúcs létrehozása.
-- -> paraméterek: ID, levél1, levél2
-- ->  3 (1, ("a", 4)) (2, ("b", 2)) -> (3, ("ab", 6))
createNode :: Id -> Node -> Node -> Node
createNode id nd1 nd2 = (id, (x1 ++ x2, y1 + y2))
    where
        x1 = fst $ snd nd1
        x2 = fst $ snd nd2
        y1 = snd $ snd nd1
        y2 = snd $ snd nd2

--Új csúcs beszúrása.
-- -> a beszúrás után a rendezettség megmarad.
-- -> (4, ("x", 1)) [(1, ("f", 1)), (2, ("m", 1)), (3, ("a", 3))] -> [(4, ("x", 1)), (1, ("f", 1)), (2, ("m", 1)), (3, ("a", 3))] 
insertNode :: Node -> [Node] -> [Node]
insertNode nd ls = sortOn (\ (x, y) -> snd y) tmp
    where
        tmp = nd : ls

--Az algoritmus egy lépése.
-- -> Ha csak egyetlen csúcs van a feldolgozandók listájában, akkor azt helyezzük át a feldolgozott csúcsok listájába.
-- -> Ha van legalább két feldolgozatlan csúcs:
--          -> 1. kivesszük a két legkisebb feldolgozatlan csúcsot: legyen x és y
--          -> 2. új csúcsot hozunk létre (legyen z) x és y-ból, az ID-t a 'nextID'-val generáljuk.
--          -> 3. címkék beállítása(Label mező): {[x -> z], [y -> z]}, {x-ből induló: '0', y-ból induló: '1'}
--          -> x,y -> feldolgozott, z -> feldolgozatlan(gyakoriság szerint!)
-- -> 
--  ([(1,("f",1)),(2,("l",1)),(3,("m",1)),(4,("a",3))],[],[]) -> 
--  ([(3,("m",1)),(5,("fl",2)),(4,("a",3))],[(1,5,"0"),(2,5,"1")],[(1,("f",1)),(2,("l",1))])
step :: State -> State
step ([], _, _) = error "Üres csúcslista"
step ([x], k, ls) = ([], k, x:ls)
step (x:y:xs, e, ls) = (tmp, edgeX:edgeY:e, x:y:ls)
    where
        --Új csomópont létrehozása.
        z = createNode id x y
        id = nextId (x:y:xs, e, ls)

        --Élek irányának megadása.
        edgeX = (fst x, id, "0") --x -> z
        edgeY = (fst y, id, "1") --y -> z

        --Új csomópont behelyezése a feldolgozatlanok közé.
        tmp = insertNode z xs

--Teljes feldolgozás.
--az előző 'step' ismétlése egészen addig amíg a feldolgozatlanok listája nem üres.
type ProcessedState = ([Edge], [Node])
steps :: State -> ProcessedState
steps state
    | null ls1 = (ed, ls2)
    | otherwise = steps (ls1, ed, ls2)
    where
        (ls1, ed, ls2) = step state

--Van-e szülő?
--ID és éllista alapján eldönti, hogy az adott ID-nak van-e szülője, azaz
-- vezet-e ki él a kapott ID-vel azonosított csúcsból.
-- -> hasParent 7 [(6,7,"0"),(4,7,"1"),(3,6,"0"),(5,6,"1"),(1,5,"0"),(2,5,"1")] -> False
-- -> hasParent 3 [(6,7,"0"),(4,7,"1"),(3,6,"0"),(5,6,"1"),(1,5,"0"),(2,5,"1")] -> True
hasParent :: Id -> [Edge] -> Bool
hasParent _ [] = False
hasParent id (x:xs)
    | aux x == id = True
    | otherwise = hasParent id xs
    where
        aux :: Edge -> Id
        aux (id, _, _) = id

--Szülőhöz vezető él.
-- -> visszaadja a szülőhöz vezető élet.
-- -> findEdgeToParent 4 [(6,7,"0"),(4,7,"1"),(3,6,"0"),(5,6,"1"),(1,5,"0"),(2,5,"1")] -> (4,7,"1")
findEdgeToParent :: Id -> [Edge] -> Edge
findEdgeToParent _ [] = error "Nincs szulo"
findEdgeToParent id (x:xs)
    | aux x == id = x
    | otherwise = findEdgeToParent id xs
    where
        aux :: Edge -> Id
        aux (id, _, _) = id

--Egy karakter kódja.
-- -> a felépített adatszerkezetből összeolvassuk egy csúcs kódját.
-- -> a kapott ID-hez tartozó csúcsból elindulunk és követjük az irányított éleket.
-- -> a kódot megkapjuk ha a címkéket összeolvassuk fordított sorrendbe.
-- -> getCodeForOne (steps $ startState $ countLetters "almafa") 1 -> "010" (az 1-es ID-u csomópont kódja)
getCodeForOne :: ProcessedState -> Id -> String
getCodeForOne pSt id
    | hasParent id tmp = getCodeForOne pSt pId ++ lb --meghivom az apa id-ra.
    | otherwise = []
    where
        tmp = fst pSt
        (mId, pId, lb) = findEdgeToParent id tmp

--Teljes kódtábla.
-- -> kiszűri azokat a csúcsokat amleyek 1 hosszú karakterből állnak.
-- -> ezekre meghatározza a kódot a 'getCodeForOne' függvény alapján.
type CodingTable = [(Char,String)]

-- -> getCodingTable $ steps $ startState $ countLetters "almafa" ->
-- -> [('a', "1"), ('m', "00"), ('f', "010"), ('l', "011")]
getCodingTable :: ProcessedState -> CodingTable
getCodingTable pSt = aux tmp
    where
        tmp = filter (\(x,y) -> length (fst y) == 1 ) (snd pSt)

        aux :: [Node] -> CodingTable
        aux [] = []
        aux (x:xs) = (ch, code) : aux xs
            where
                id = fst x
                ch = head $ fst $ snd x
                code = getCodeForOne pSt id

--Keresés a kódtáblában.
-- -> egy adott karakterhez tartozó kód megkeresése a kódtáblában.
-- -> findCode 'l' [('a', "1"), ('m', "00"), ('f', "010"), ('l', "011")] -> "011"
findCode :: Char -> CodingTable -> String
findCode ch (x:xs)
    | ch == fst x = snd x
    | otherwise = findCode ch xs

--Kódolás.
-- A kódolás menete:
--      -> karakter-előfordulási statisztika 'countLetters'. [1]
--      -> kezdőállapot meghatározása 'startState'. [2]
--      -> csúcsösszevonó algoritmus 'steps'. [3]
--      -> kódtábla felépítése 'getCodingTable'. [4]
--      -> a kódtáblából kikeresve a kódokat felépítsük a kódot. [final]

-- -> fst $ encode "almafa" -> "10110010101"
encode :: String -> (String, CodingTable) -- -> visszadja a kódot és a kódtáblát is.
encode str = (aux str, tmp)
    where
        --[1] - [4]
        tmp = getCodingTable $ steps $ startState $ countLetters str

        --[final]
        aux :: String -> String
        aux [] = []
        aux (x:xs) = findCode x tmp ++ aux xs

--Kereses a kódtáblában (folytatás).
-- -> kikeresi a kódtáblában egy megadott kódhoz tartozó karaktert.
-- -> findChar "00" [('a', "1"), ('m', "00"), ('f', "010"), ('l', "011")] -> 'm'
findChar :: String -> CodingTable -> Char
findChar code (x:xs)
    | code == snd x = fst x
    | otherwise = findChar code xs

--Dekódolás.
-- A dekódolás menete:
--      -> keressük meg a kódtáblában azt a kódot, amelyik prefixe a paraméterül kapott kódsorozatnak. 
--         Ez lesz a dekódolt szöveg első karaktere, melyet a findChar függvénnyel deríthetünk ki. [1]
--      -> rekurzívan ismételjük ameddig elfogy a kód, az előzőleg visszafejtett szöveg elé tegyük a kapott karaktert. [2]
-- szükséges a visszafejtéshez a kódtábla.
-- -> decode (fst $ encode "almafa") (snd $ encode "almafa") -> "almafa"
decode :: String -> CodingTable -> String
decode str cTbl
    | null str = []
    | otherwise = ch : decode (str \\ pfx) cTbl --[2]
    where
        aux :: CodingTable -> String
        aux (x:xs)
            | snd x `isPrefixOf` str = snd x
            | otherwise = aux xs

        --[1] -> prefix majd a karakter kikeresése.
        pfx = aux cTbl
        ch = findChar pfx cTbl