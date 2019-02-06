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

backwardsCheck :: (Ord st, Ord sy) => Automaton st sy -> [sy] -> st -> Bool
backwardsCheck fsa str q =
    let (start, ends, delta) = fsa in
    let states = allStates fsa in
    case str of
ε -> q == start
x1 x2 . . . xn−1 xn -> or (map (\q’ -> forwardCheck fsa x1 x2 . . . xn−1 q’ && elem
(q’,xn ,q)
delta) states)

