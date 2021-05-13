{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    state :: s,
    action :: Maybe a,
    parent :: Maybe (Node s a),
    depth :: Int,
    heuristic :: Float,
    children :: [Node s a]
}

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where 
    node1 == node2 = (state node1) == (state node2)
    _ == _ = False

instance Ord s => Ord (Node s a) where
    node1 <= node2 = (state node1) <= (state node2)
    _ <= _ = False

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState node = (state node)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = (parent node)

nodeDepth :: Node s a -> Int
nodeDepth node = (depth node)

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = (children node)

nodeHeuristic :: Node s a -> Float
nodeHeuristic node = (heuristic node)

nodeAction :: Node s a -> Maybe a
nodeAction node = (action node) 

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpaceHelper :: (ProblemState s a, Eq s) => s -> a -> Maybe (Node s a) -> Int -> Float -> Node s a
createStateSpaceHelper state action parent depth heu = currentNode
    where currentNode = Node state (Just action) parent depth heu children
          children = (foldl (\acc x -> acc ++ [createStateSpaceHelper (snd x) (fst x) (Just currentNode) (depth + 1)
            (h (snd x))]) [] succ)
          succ = (successors state)

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = currentNode
    where currentNode = Node initialState Nothing Nothing 0 (h initialState) children
          children = (foldl (\acc x -> acc ++ [createStateSpaceHelper (snd x) (fst x) (Just currentNode) 1
            (h (snd x))]) [] succ)
          succ = (successors initialState)

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

findInSet :: (ProblemState s a, Ord s) => (Node s a) -> (S.Set s) -> Bool
findInSet node set =
    S.foldl (\acc x -> if ((nodeState node) == x) then True else acc) False set

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = 
    let state = nodeState node
        succs = nodeChildren node
    in foldl (\acc x -> if (not (findInSet x visited)) then acc ++ [x] else acc) [] succs

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

costFunction :: Int -> Float -> Float
costFunction depth heuristic = (fromIntegral depth :: Float) + heuristic

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = newFrontier
    where heuristic = (nodeHeuristic node)
          depth = (nodeDepth node)
          cost = (costFunction depth heuristic)
          newFrontier = PQ.insertWith min node cost frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = newFrontier
    where newFrontier = foldl insertSucc frontier validsuccessors
          validsuccessors = suitableSuccs node visited 

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier =
    if (isGoal (nodeState (fst node)))
        then fst node
        else astar' (S.insert (nodeState (fst node)) visited) (insertSuccs (fst node) (snd $ deleteFindMin frontier) (S.insert (nodeState (fst node)) visited))
    where succs = suitableSuccs (fst node) visited
          node = deleteFindMin frontier

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' (S.empty) (insertSucc PQ.empty initialNode) 

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

fromMaybeNodeToNode :: Maybe (Node s a) -> Node s a
fromMaybeNodeToNode (Just node) = node

fromMaybeActionToAction :: Maybe a -> a
fromMaybeActionToAction (Just action) = action

funcIterate :: Node s a -> Node s a
funcIterate node = fromMaybeNodeToNode (nodeParent node)

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = list
    where list = foldl (\acc x -> [(fromMaybeActionToAction (nodeAction x), nodeState x)] ++ acc) [] listValid
          listValid = takeWhile (\x -> isJust (nodeParent x)) listNodes
          listNodes = iterate funcIterate goalNode