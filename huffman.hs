--Huffman kodolas
import Data.List ( (\\), group, isPrefixOf, sort, sortOn )
import Data.Ord ()
import System.IO () 

type Frequency = Int
type LetterSum = (String, Frequency)

--Betugyakorisag.
countLetters :: String -> [LetterSum]
countLetters str = map (\x -> ([head x], length x)) $ group $ sort str

--Kezdoallapot.
--A fa abrazolasahoz az alabbi tipusokat hasznaljuk.
type Id    = Int
type Node  = (Id, LetterSum)
type Label = String
type Edge  = (Id, Id, Label)

--Az algoritmus kezdoallapota.
type State = ([Node], [Edge], [Node])
startState :: [LetterSum] -> State
startState ltSum = (aux tmpLs 1, [], [])
    where
        aux :: [LetterSum] -> Int -> [Node]
        aux [] _ = []
        aux (x:xs) idx = (idx, x) : aux xs (idx+1)  

        tmpLs = sortOn snd ltSum 

--Kovetkezo azonosito eloallitasa.
nextId :: State -> Id
nextId (ls, _, _) = aux ls + 1
    where
        aux :: [Node] -> Id
        aux [x] = fst x
        aux (x:xs)
            | fst x > aux xs = fst x
            | otherwise = aux xs

--Uj csucs letrehozasa.
createNode :: Id -> Node -> Node -> Node
createNode id nd1 nd2 = (id, (x1 ++ x2, y1 + y2))
    where
        x1 = fst $ snd nd1
        x2 = fst $ snd nd2
        y1 = snd $ snd nd1
        y2 = snd $ snd nd2

--Uj csucs beszurasa.
insertNode :: Node -> [Node] -> [Node]
insertNode nd ls = sortOn (\ (x, y) -> snd y) tmp
    where 
        tmp = nd : ls

--Az algoritmus egy lepese.
step :: State -> State
step ([], _, _) = error "Ures csucslista"
step ([x], k, ls) = ([], k, x:ls)
step (x:y:xs, e, ls) = (tmp, edgeX:edgeY:e, x:y:ls)
    where
        --uj csomopont letrehozasa.
        z = createNode id x y 
        id = nextId (x:y:xs, e, ls)

        --elek iranyanak megadasa.
        edgeX = (fst x, id, "0") --x -> z
        edgeY = (fst y, id, "1") --y -> z

        --uj csomopont behelyezes a feldolgoztlanok koze.
        tmp = insertNode z xs

--Teljes feldolgozas.
type ProcessedState = ([Edge], [Node])
steps :: State -> ProcessedState
steps state
    | null ls1 = (ed, ls2)
    | otherwise = steps (ls1, ed, ls2)
    where
        (ls1, ed, ls2) = step state

--Van-e szulo?
hasParent :: Id -> [Edge] -> Bool
hasParent _ [] = False
hasParent id (x:xs)
    | aux x == id = True
    | otherwise = hasParent id xs
    where
        aux :: Edge -> Id
        aux (id, _, _) = id

--Szulohoz vezeto el.
findEdgeToParent :: Id -> [Edge] -> Edge
findEdgeToParent _ [] = error "Nincs szulo"
findEdgeToParent id (x:xs)
    | aux x == id = x
    | otherwise = findEdgeToParent id xs
    where
        aux :: Edge -> Id
        aux (id, _, _) = id

--Egy karakter kodja.
getCodeForOne :: ProcessedState -> Id -> String
getCodeForOne pSt id
    | hasParent id tmp = getCodeForOne pSt pId ++ lb --meghivom az apa id-ra.
    | otherwise = []
    where
        tmp = fst pSt
        (mId, pId, lb) = findEdgeToParent id tmp

--Teljes kodtabla.
type CodingTable = [(Char,String)]

getCodingTable :: ProcessedState -> CodingTable
getCodingTable pSt = aux tmp
    where
        --az egykarakteresek kiszurese.
        tmp = filter (\(x,y) -> length (fst y) == 1 ) (snd pSt)

        aux :: [Node] -> CodingTable
        aux [] = []
        aux (x:xs) = (ch, code) : aux xs
            where
                id = fst x
                ch = head $ fst $ snd x
                code = getCodeForOne pSt id
        
--Kereses a kodtablaban.
findCode :: Char -> CodingTable -> String
findCode ch (x:xs) 
    | ch == fst x = snd x 
    | otherwise = findCode ch xs

--Kodolas.
--Visszaadja a CodingTable-t is.
encode :: String -> (String, CodingTable)
encode str = (aux str, tmp)
    where
        tmp = getCodingTable $ steps $ startState $ countLetters str

        aux :: String -> String
        aux [] = []
        aux (x:xs) = findCode x tmp ++ aux xs

--Kereses a kodtablaban (folytatas).
findChar :: String -> CodingTable -> Char
findChar code (x:xs) 
    | code == snd x = fst x 
    | otherwise = findChar code xs

--Dekodolas.
--Ismerni kell a CodingTable-t.
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
    
--Meghivasok:
--Kodolas: fst $ encode "Sapientia"
--Dekodolas: decode (fst $ encode "Sapientia") (snd $ encode "Sapientia") -> 3. para a CodingTable

main :: IO ()
main = do
    putStrLn "Olvasson be egy karakterlancot: "
    z <- getLine
    let (enc, dec) = encode z
    putStrLn $ "A kodolt szoveg: " ++ enc
    putStrLn $ "A visszafejtett szoveg: " ++ decode enc dec