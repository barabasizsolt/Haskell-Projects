--Huffman kódolás.
import Data.List ( (\\), group, isPrefixOf, sort, sortOn )
import Data.Ord ()
import System.IO () 

type Frequency = Int
type LetterSum = (String, Frequency)

--betűgyakoriság meghatározása.
countLetters :: String -> [LetterSum]
countLetters str = map (\x -> ([head x], length x)) $ group $ sort str

--Kezdőállapot meghatározása.
--A Huffman fa ábrázolása az alábbi típusok segítségével.
type Id    = Int
type Node  = (Id, LetterSum)
type Label = String
type Edge  = (Id, Id, Label)

--Az algoritmus kezdőállapota.
type State = ([Node], [Edge], [Node])
startState :: [LetterSum] -> State
startState ltSum = (aux tmpLs 1, [], [])
    where
        aux :: [LetterSum] -> Int -> [Node]
        aux [] _ = []
        aux (x:xs) idx = (idx, x) : aux xs (idx+1)  

        tmpLs = sortOn snd ltSum 

--Követekező azonosító előállítása.
nextId :: State -> Id
nextId (ls, _, _) = aux ls + 1
    where
        aux :: [Node] -> Id
        aux [x] = fst x
        aux (x:xs)
            | fst x > aux xs = fst x
            | otherwise = aux xs

--Új csúcs létrehozása.
createNode :: Id -> Node -> Node -> Node
createNode id nd1 nd2 = (id, (x1 ++ x2, y1 + y2))
    where
        x1 = fst $ snd nd1
        x2 = fst $ snd nd2
        y1 = snd $ snd nd1
        y2 = snd $ snd nd2

--Új csúcs beszúrása.
insertNode :: Node -> [Node] -> [Node]
insertNode nd ls = sortOn (\ (x, y) -> snd y) tmp
    where 
        tmp = nd : ls

--Az algoritmus egy lépése
step :: State -> State
step ([], _, _) = error "Ures csucslista"
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
type ProcessedState = ([Edge], [Node])
steps :: State -> ProcessedState
steps state
    | null ls1 = (ed, ls2)
    | otherwise = steps (ls1, ed, ls2)
    where
        (ls1, ed, ls2) = step state

--Van-e szülő?
hasParent :: Id -> [Edge] -> Bool
hasParent _ [] = False
hasParent id (x:xs)
    | aux x == id = True
    | otherwise = hasParent id xs
    where
        aux :: Edge -> Id
        aux (id, _, _) = id

--Szülőhöz vezető él.
findEdgeToParent :: Id -> [Edge] -> Edge
findEdgeToParent _ [] = error "Nincs szulo"
findEdgeToParent id (x:xs)
    | aux x == id = x
    | otherwise = findEdgeToParent id xs
    where
        aux :: Edge -> Id
        aux (id, _, _) = id

--Egy karakter kódja.
getCodeForOne :: ProcessedState -> Id -> String
getCodeForOne pSt id
    | hasParent id tmp = getCodeForOne pSt pId ++ lb --meghivom az apa id-ra.
    | otherwise = []
    where
        tmp = fst pSt
        (mId, pId, lb) = findEdgeToParent id tmp

--Teljes kódtábla.
type CodingTable = [(Char,String)]

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
findCode :: Char -> CodingTable -> String
findCode ch (x:xs) 
    | ch == fst x = snd x 
    | otherwise = findCode ch xs

--Kódolás.
encode :: String -> (String, CodingTable)
encode str = (aux str, tmp)
    where
        tmp = getCodingTable $ steps $ startState $ countLetters str

        aux :: String -> String
        aux [] = []
        aux (x:xs) = findCode x tmp ++ aux xs

--Kereses a kódtáblában (folytatás).
findChar :: String -> CodingTable -> Char
findChar code (x:xs) 
    | code == snd x = fst x 
    | otherwise = findChar code xs

--Dekódolás.
decode :: String -> CodingTable -> String
decode str cTbl 
    | null str = []
    | otherwise = ch : decode (str \\ pfx) cTbl
    where
        aux :: CodingTable -> String
        aux (x:xs)
            | snd x `isPrefixOf` str = snd x
            | otherwise = aux xs 

        pfx = aux cTbl
        ch = findChar pfx cTbl 
    
main :: IO ()
main = do
    putStrLn "Olvasson be egy karakterlancot: "
    z <- getLine
    let (enc, dec) = encode z
    putStrLn $ "A kodolt szoveg: " ++ enc
    putStrLn $ "A visszafejtett szoveg: " ++ decode enc dec
