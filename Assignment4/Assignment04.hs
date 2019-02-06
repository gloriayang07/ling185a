module Assignment04 where

import FiniteState
import FiniteStatePart2

---------------------------------------
-- Setup for section 2

type SLG sy = ([sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Ord, Show)

slg1 :: SLG SegmentCV
slg1 = ([C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 3

data RegExp sy = Lit sy
               | Alt (RegExp sy) (RegExp sy)
               | Concat (RegExp sy) (RegExp sy)
               | Star (RegExp sy)
               | ZeroRE
               | OneRE
               deriving Show

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3))

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

-- Produces a new version of an FSA with the guarantee that certain 
-- integers are not used as state labels.
ensureUnused :: [Int] -> EpsAutomaton Int sy -> EpsAutomaton Int sy
ensureUnused reserved fsa =
    if reserved == [] then
        fsa
    else
        -- nonneg maps integers to non-negative integers, preserving all distinctions
        let nonneg x = if x < 0 then 2*(-x)-1 else 2*x in
        -- create a version of fsa where all state numbers are non-negative
        let fsanonneg = mapStates nonneg fsa in
        -- now add enough to all state numbers to make sure they don't clash with the reserved list
        mapStates (\x -> x + 1 + maximum reserved) fsanonneg

-- Adjusts the state labels throughout an FSA
mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f (start, ends, delta) =
    let newDelta = map (\(q1,x,q2) -> (f q1, x, f q2)) delta in
    (f start, map f ends, newDelta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
------------------------------------------------------------------

-- 1
backwardCheck :: (Ord st, Ord sy) => Automaton st sy -> [sy] -> st -> Bool
backwardCheck fsa str q =
    let (start, ends, delta) = fsa in
    let states = allStates fsa in
    case str of
        [] -> elem q ends
        x : rest -> or (map (\q' -> backwardCheck fsa rest q' && elem (q, x, q') delta) states)

-- 2.1 helper function
recursiveRecognizeSLG :: (Ord sy) => SLG sy -> [sy] -> Bool
recursiveRecognizeSLG slg str =
    let (s, f, t) = slg in
    case str of
        [] -> True -- we're reached the end of the list
        x : rest -> case rest of
                    [] -> elem x f -- here x is the last symbol
                    y : tail -> elem (x,y) t && recursiveRecognizeSLG slg rest

-- 2.1
recognizeSLG :: (Ord sy) => SLG sy -> [sy] -> Bool
recognizeSLG slg str =
    let (s, f, t) = slg in
    case str of
        [] -> False
        x : rest -> elem x s && recursiveRecognizeSLG slg str

-- 2.2
-- ExtraState is the start state
-- Array of end states is an array of (StateForSymbol sy), where sy is every element in f
-- Transition from (ExtraState, sy, StateForSymbol sy), where sy is every element in s
-- Transition from (StateForSymbol sy1, sy2, StateForSymbol sy2) for every (sy1, sy2) in t
slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA slg =
    let (s, f, t) = slg in
    let startState = ExtraState in
    let endStates = map (\elemF -> StateForSymbol elemF) f in
    let startTransitions = map (\elemS -> (ExtraState, elemS, StateForSymbol elemS)) s in
    let otherTransitions = map (\(sy1, sy2) -> (StateForSymbol sy1, sy2, StateForSymbol sy2)) t in
        (startState, endStates, startTransitions ++ otherTransitions)

-- 3: union
unionFSAs :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy -> EpsAutomaton Int sy
unionFSAs m m' =
    let startState = 40 in
    let (start, ends, delta) = ensureUnused [startState] m in
    let (start', ends', delta') = ensureUnused ((allStates m) ++ [startState]) m' in
    let newStart = startState in
    let newEnds = ends ++ ends' in
    let newStartTransitions = [(newStart, Nothing, start), (newStart, Nothing, start')] in
    let newDelta = newStartTransitions ++ delta ++ delta' in
        (newStart, newEnds, newDelta)

-- 3: concat
concatFSAs :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy -> EpsAutomaton Int sy
concatFSAs m m' =
    let (start, ends, delta) = m in
    let (start', ends', delta') = ensureUnused (allStates m) m' in
    let newStart = start in
    let newEnds = ends' in
    let connectionTransitions = map (\endState -> (endState, Nothing, start')) ends in
    let newDelta = connectionTransitions ++ delta ++ delta' in
        (newStart, newEnds, newDelta)

-- 3: star
starFSA :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy
starFSA m =
    let startState = 40 in
    let (start, ends, delta) = ensureUnused [startState] m in
    let newStart = startState in
    let newEnds = ends ++ [newStart] in
    let newStartTransition = [(newStart, Nothing, start)] in
    let newOtherTransitions = map (\endState -> (endState, Nothing, start)) ends in
    let newDelta = newStartTransition ++ newOtherTransitions ++ delta in
        (newStart, newEnds, newDelta)

-- 3: reToFSA
reToFSA :: (Ord sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA r =
    let literalStart = 50 in
    let literalEnd = 51 in
    let zeroState = 52 in
    let oneState = 53 in
    let avoidThese = [literalStart, literalEnd, zeroState, oneState] in
                case r of
                Lit x -> (literalStart, [literalEnd], [(literalStart, Just x, literalEnd)])
                Alt r1 r2 -> unionFSAs (ensureUnused avoidThese (reToFSA r1)) (ensureUnused avoidThese (reToFSA r2))
                Concat r1 r2 -> concatFSAs (ensureUnused avoidThese (reToFSA r1)) (ensureUnused avoidThese (reToFSA r2))
                Star r -> starFSA (ensureUnused avoidThese (reToFSA r))
                ZeroRE -> (zeroState, [], [])
                OneRE -> (oneState, [oneState], [])