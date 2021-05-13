{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    rows :: Int,
    column :: Int,
    targets :: [Target],
    hunter :: Position,
    obstacles :: [Position],
    gateways :: [(Position, Position)]
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

modifyStringTargets :: Int -> String -> Target -> String
modifyStringTargets columns string target@(Target position behavior) = 
     let row = fst position
         column = snd position
         pos = row * (columns + 1) + column + 1
     in (take (pos - 1) string) ++ ['*'] ++ (drop pos string)


putElem :: Int -> Char -> String -> Position -> String
putElem columns elem string hunter =
    let row = fst hunter
        column = snd hunter
        pos = row * (columns + 1) + column + 1
    in (take (pos - 1) string) ++ [elem] ++ (drop pos string)

putElemPair :: Int -> Char -> String -> (Position, Position) -> String
putElemPair columns elem string gate =
    let row1 = fst $ fst gate
        column1 = snd $ fst gate
        row2 = fst $ snd gate
        column2 = snd $ snd gate
        pos1 = row1 * (columns + 1) + column1 + 1
        pos2 = row2 * (columns + 1) + column2 + 1
        string1 = (take (pos1 - 1) string) ++ [elem] ++ (drop pos1 string)
    in (take (pos2 - 1) string1) ++ [elem] ++ (drop pos2 string1)

putGameObstacles :: Int -> Char -> [Position] -> String -> String
putGameObstacles columns elem pos string = foldl (putElem columns elem) string pos

putGameGateways :: Int -> Char -> [(Position, Position)] -> String -> String
putGameGateways columns elem pos string = foldl (putElemPair columns elem) string pos

putTargets :: [Target] -> String -> Int -> String
putTargets targets string columns = 
    foldl (modifyStringTargets columns) string targets

gameAsString :: Game -> String
gameAsString game@(Game rows columns targets hunter obstacles gateways) =
     let string = (take ((rows - 1) * (columns + 1)) $ (cycle ((take columns $ cycle [' ']) ++ ['\n']))) ++ (take columns $ cycle [' '])
     in putTargets targets (putElem columns '!' (putGameGateways columns '#' gateways (putGameObstacles columns '@' obstacles string)) hunter) columns

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}

makeObstacles :: Int -> Int -> [Position]
makeObstacles rows columns = (zip (cycle [0]) [0..(columns - 1)]) ++
    (zip [1..(rows - 1)] (cycle [0])) ++ (zip [1..(rows - 1)] (cycle [columns - 1])) ++
    (zip (cycle [rows - 1]) [0..(columns - 1)])

emptyGame :: Int -> Int -> Game
emptyGame rows columns = Game rows columns [] (1, 1) (makeObstacles rows columns) []

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

invalidPositionHunter :: Position -> Game -> Bool
invalidPositionHunter pos game@(Game rows columns targets hunter obstacles gateways)
        | position <= (length string) = (if (head (drop (position - 1) string) == ' ') then False
                                            else True)
        | otherwise = True
        where row = fst pos
              column = snd pos
              position = row * (columns + 1) + column + 1
              string = gameAsString game

invalidPosition :: Position -> Game -> Bool
invalidPosition pos game@(Game rows columns targets hunter obstacles gateways)
        | position <= (length string) = False
        | otherwise = True
        where row = fst pos
              column = snd pos
              position = row * (columns + 1) + column + 1
              string = gameAsString game

addHunter :: Position -> Game -> Game
addHunter pos game@(Game rows columns targets hunter obstacles gateways)
    | invalidPositionHunter pos game = game
    | otherwise = Game rows columns targets pos obstacles gateways

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behavior position game@(Game rows columns targets hunter obstacles gateways)
    | invalidPosition position game = game
    | otherwise = Game rows columns (targets ++ [(Target position behavior)]) hunter obstacles gateways

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}

addGateway :: (Position, Position) -> Game -> Game
addGateway gateway@((pos1, pos2)) game@(Game rows columns targets hunter obstacles gateways)
    | invalidPosition pos1 game = game
    | invalidPosition pos2 game = game
    | otherwise = Game rows columns targets hunter obstacles (gateways ++ [gateway])

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game@(Game rows columns targets hunter obstacles gateways)
    | invalidPosition pos game = game
    | otherwise = Game rows columns targets hunter (obstacles ++ [pos]) gateways
{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

checkGateways :: Position -> Maybe Position -> (Position, Position) -> Maybe Position
checkGateways p acc g
    | ((fst p) == (fst (fst g)) && ((snd p) == (snd (fst g)))) = (Just (snd g))
    | ((fst p) == (fst (snd g)) && ((snd p) == (snd (snd g)))) = (Just (fst g))
    | otherwise = acc

searchForDestination :: Position -> [(Position, Position)] -> Maybe Position
searchForDestination pos gateways = foldl (checkGateways pos) Nothing gateways

attemptMove :: Position -> Game -> Maybe Position
attemptMove position game@(Game rows columns targets hunter obstacles gateways)
    | elem == ' ' = Just position
    | elem == '#' = searchForDestination position gateways
    | elem == '@' = Nothing
    | otherwise = Nothing
    where row = fst position
          column = snd position
          pos = row * (columns + 1) + column + 1
          string = gameAsString game
          elem = head (drop (pos - 1) string)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

invalidPositionTargetDirection :: Position -> Game -> Bool
invalidPositionTargetDirection pos game@(Game rows columns targets hunter obstacles gateways)
        | position <= (length string) = (if ((head (drop (position - 1) string) == ' ') ||
                                            (head (drop (position - 1) string) == '#')) then False
                                            else True)
        | otherwise = True
        where row = fst pos
              column = snd pos
              position = row * (columns + 1) + column + 1
              string = gameAsString game

invalidPositionGateways :: Position -> Game -> Int
invalidPositionGateways pos game@(Game rows columns targets hunter obstacles gateways) =
    foldl (\acc x -> if (((fst pos) == (fst (snd x))) && (snd pos == (snd (snd x))) || 
        ((fst pos) == (fst (fst x))) && (snd pos == (snd (fst x)))) then (acc + 1) else acc) 0 gateways

computePosition :: Int -> Int -> Position -> Position
computePosition direction columns pos
    | direction == 0 = (fst pos, (snd pos) + 1)
    | direction == 1 = (fst pos, (snd pos) - 1)
    | direction == 2 = ((quot (position - columns - 1) (columns + 1)), (mod (position - columns - 1) (columns + 1)) - 1)
    | otherwise = ((quot (position + columns + 1) (columns + 1)), (mod (position + columns + 1) (columns + 1)) - 1)
    where position = (fst pos) * (columns + 1) + (snd pos) + 1

findTargetBehavior :: Position -> [Target] -> Behavior
findTargetBehavior pos targets = foldl (\acc x -> if (((fst (position x)) == (fst pos)) && 
    ((snd (position x)) == (snd pos))) then (behavior x) else acc) goEast targets

fromMaybePosToPos :: Maybe Position -> Position
fromMaybePosToPos Nothing = (-1, -1)
fromMaybePosToPos (Just pos) = pos

goDirection :: Int -> Position -> Game -> Target
goDirection dir pos game@(Game rows columns targets hunter obstacles gateways)
    | invalidPositionTargetDirection position game = 
            if ((invalidPositionGateways pos game) == 0)
                then (Target pos (findTargetBehavior pos targets))
                else (Target (fromMaybePosToPos (searchForDestination pos gateways)) (findTargetBehavior pos targets))
    | otherwise = if ((invalidPositionGateways position game) == 0)
        then (Target position (findTargetBehavior pos targets))
        else (Target (fromMaybePosToPos (searchForDestination position gateways)) (findTargetBehavior pos targets))
    where string = gameAsString game
          position = computePosition dir columns pos
          pos_aux = (fst position) * (columns + 1) + (snd position) + 1

computePositionBounce :: Int -> Int -> Position -> Position
computePositionBounce direction columns pos
    | direction == 1 = ((quot (position - columns - 1) (columns + 1)), (mod (position - columns - 1) (columns + 1)) - 1)
    | otherwise = ((quot (position + columns + 1) (columns + 1)), (mod (position + columns + 1) (columns + 1)) - 1)
    where position = (fst pos) * (columns + 1) + (snd pos) + 1

goDirectionBounce :: Int -> Position -> Game -> Target
goDirectionBounce dir pos game@(Game rows columns targets hunter obstacles gateways)
    | ((invalidPositionTargetDirection position game) && ((invalidPositionGateways pos game) == 0) && dir == 1) = 
        (Target (computePositionBounce (-1) columns pos) (bounce (-1)))
    | ((invalidPositionTargetDirection position game) && ((invalidPositionGateways pos game) == 0) && dir == -1) = 
        (Target (computePositionBounce 1 columns pos) (bounce 1))
    | ((invalidPositionTargetDirection position game) && ((invalidPositionGateways pos game) /= 0) && dir == 1) =        
        (Target (computePositionBounce (-1) columns pos) (bounce (-1)))
    | ((invalidPositionTargetDirection position game) && ((invalidPositionGateways pos game) /= 0) && dir == -1) =        
        (Target (computePositionBounce 1 columns pos) (bounce 1))
    | ((not (invalidPositionTargetDirection position game)) && ((invalidPositionGateways position game) == 0)) =
        (Target position (findTargetBehavior pos targets))
    | otherwise =
        (Target (fromMaybePosToPos (searchForDestination position gateways)) (findTargetBehavior pos targets))
    where string = gameAsString game
          position = computePositionBounce dir columns pos
          pos_aux = (fst position) * (columns + 1) + (snd position) + 1

goEast :: Behavior
goEast = goDirection 0

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest = goDirection 1

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth = goDirection 2

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth = goDirection 3
{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce dir
    | dir == 1 = goDirectionBounce 1
    | otherwise = goDirectionBounce (-1)

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game@(Game rows columns targets hunter obstacles gateways) =
    Game rows columns (foldl (\acc x -> acc ++ [((behavior x) (position x) game)]) [] targets)
    hunter obstacles gateways

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled position1 target
    | (fst position1) == (fst position2) =
        if ((abs ((snd position1) - (snd position2))) == 1) then True else False
    | (snd position1) == (snd position2) =
        if ((abs ((fst position1) - (fst position2))) == 1) then True else False
    | otherwise = False
    where position2 = (position target)


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

directionToInt :: Direction -> Int
directionToInt dir
    | dir == North = 2
    | dir == South = 3
    | dir == West = 1
    | otherwise = 0

eliminateTargets :: Game -> Game
eliminateTargets game@(Game rows columns targets hunter obstacles gateways) =
    Game rows columns newTargets hunter obstacles gateways
    where newTargets = foldl (\acc x -> if (not (isTargetKilled hunter x)) then acc ++ [x] else acc) [] targets

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir flag game@(Game rows columns targets hunter obstacles gateways)
    | flag == True = eliminateTargets $ moveTargets $ eliminateTargets game_hunter
    | flag == False = game_hunter
    where pos = computePosition (directionToInt dir) columns hunter
          may = attemptMove pos game
          game_hunter = if (may == Nothing) then game
                    else Game rows columns targets (fromMaybePosToPos may) obstacles gateways

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game@(Game rows columns targets hunter obstacles gateways)
    | (length targets) /= 0 = True
    | otherwise = False


{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}

{- 
    Position -> Int -> Position -> Game -> Target
    IMPORTANT!!
    Comenzi de testare:
    -- interactive $ loadGame "terrains/circle.txt" [circle (4, 9) 2] []
    -- interactive $ loadGame "terrains/circle.txt" [circle (4, 9) 3] []
    (a doua poate fi data cu conditia sa fie schimbata pozitia targetului
    pe tabla de joc)
-}
circle :: Position -> Int -> Behavior
circle pos r t game@(Game rows columns targets hunter obstacles gateways)
    | (not ((snd pos) - y >= aux)) && ((fst pos) - x == aux + 1) = Target (x, y - 1) (circle pos r) -- mergem catre est
    | ((snd pos) - y) == aux && x < (fst pos) = Target (x + 1, y - 1) (circle pos r) -- mergem pe diagonala sud-vest 
    | (not ((x - (fst pos)) >= aux)) && y < (snd pos) = Target (x + 1, y) (circle pos r) -- merg in jos
    | (x - (fst pos)) == aux && y < (snd pos) = Target (x + 1, y + 1) (circle pos r) -- merg spre sud-est
    | (x > (fst pos)) && (not ((y - (snd pos)) >= aux)) = Target (x, y + 1) (circle pos r) -- merg spre est
    | (y - (snd pos)) == aux && (x > (fst pos)) = Target (x - 1, y + 1) (circle pos r) -- merg spre nord-est
    | y > (snd pos) && (not ((fst pos) - x >= aux)) = Target (x - 1, y) (circle pos r) -- merg spre nord
    | (fst pos) - x == aux && y > (snd pos) = Target (x - 1, y - 1) (circle pos r) -- merg spre nord-vest
    | otherwise = Target t (circle pos r)
    where x = fst t
          y = snd t
          line = 2 * r - 1
          aux = (line - 1) `div` 2

isValidSuccessor :: Game -> (Direction, Game) -> Bool
isValidSuccessor game pair
    | game == snd pair = False
    | otherwise = True

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}

    successors game@(Game rows columns targets hunter obstacles gateways) =
        [(North, advanceGameState North False game)] ++ [(South, advanceGameState South False game)] ++
        [(East, advanceGameState East False game)] ++ [(West, advanceGameState West False game)]

    {-  
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game@(Game rows columns targets hunter obstacles gateways) =
        if (isTargetKilled hunter target) then True
            else False
        where target = head targets
    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game@(Game rows columns targets hunter obstacles gateways) = 
        hEuclidean hunter (position target)
        where target = head targets

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

fromJustTarget :: Maybe Target -> Target
fromJustTarget (Just target) = target

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame game@(Game rows columns targets hunter obstacles gateways)) =
        [(North, (BonusGame (advanceGameState North False game)))] ++ [(South, (BonusGame (advanceGameState South False game)))] ++
        [(East, (BonusGame (advanceGameState East False game)))] ++ [(West, (BonusGame (advanceGameState West False game)))]

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame game@(Game rows columns targets hunter obstacles gateways)) =
        if (isTargetKilled hunter target) then True
            else False
        where target = head targets

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}

    -- h (BonusGame game@(Game rows columns targets hunter obstacles gateways)) =
    --     hEuclidean hunter (position target)
    --     where target = foldl (\acc x -> if ((hEuclidean hunter (position x)) > (hEuclidean hunter (position acc))) then acc else x) (head targets) (tail targets)

    -- h (BonusGame game@(Game rows columns targets hunter obstacles gateways)) =
    --     hEuclidean hunter (position target)
    --     where target = foldl (\acc x -> if (((hEuclidean hunter (position x)) > (hEuclidean hunter (position acc))) && ((hEuclidean hunter (position x)) > ((hEuclidean hunter (position (behavior x (position x) game)))))) then acc else x) (head targets) (tail targets)

    h g@(BonusGame game@(Game rows columns targets hunter obstacles gateways))
        | target == Nothing = 100
        | otherwise = hEuclidean hunter (position (fromJustTarget target))
        where target = if (isGoal g) then (Just $ head targets) else Nothing