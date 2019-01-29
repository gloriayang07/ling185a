module FiniteState where

import Data.Set (fromList, toList)

-- A little trick for eliminating duplicates from a list
set :: (Ord a) => [a] -> [a]
set xs = toList (fromList xs)

------------------------------------------

data SegmentCV = C | V deriving (Show, Eq, Ord)

-- Corresponds to (5) on the handout. (The first two components 
-- of the five-tuple correspond to the type argument st and sy.)
type Automaton st sy = (st, [st], [(st,sy,st)])

-- Correspondes to (4)/(6) on the handout. 
-- Requires two adjacent Cs or two adjacent Vs.
fsa_handout4 :: Automaton Int SegmentCV
fsa_handout4 = (40, [43], [(40, C, 40),
                           (40, V, 40),
                           (40, C, 41),
                           (40, V, 42),
                           (41, C, 43),
                           (42, V, 43),
                           (43, C, 43),
                           (43, V, 43)])

-------------------------------------------------------------------------------------------------

-- Given a transition table, a state and a symbol, find all the states 
-- we can get to by stepping away from that state and emitting that symbol.
targets :: (Ord st, Ord sy) => [(st,sy,st)] -> st -> sy -> [st]
targets delta q     x =
    case delta of
    [] -> []
    (firstTriple:rest) ->
        let (q1,y,q2) = firstTriple in
        if q1 == q && y == x then
            q2 : (targets rest q x)
        else
            targets rest q x

-- Alternative definition of targets, i.e. Delta_3, which matches 
-- the definition in (8) on the handout.
targets' :: (Ord st, Ord sy) => [(st,sy,st)] -> st -> sy -> [st]
targets' delta q x = map (\(q1,y,q2) -> q2) (
                        filter (\(q1,y,q2) -> q1 == q && y == x) delta
                     )

-- Corresponds directly to the definition in (13) in the handout: 
-- hat takes a transition-table delta which specifies single-symbol transitions, 
-- and extends it to a function that specifies ``multi-symbol transitions''.
hat :: (Ord st, Ord sy) => [(st,sy,st)] -> st -> [sy] -> [st]
hat delta q w =
    case w of
    [] -> [q]
    (x:u) -> set (concat (map (\q' -> hat delta q' u) (targets' delta q x)))

-- Corresponds to the definition in (15), roughly: this function 
-- will return True iff the given string is in the set defined there.
recognize :: (Ord st, Ord sy) => Automaton st sy -> [sy] -> Bool
recognize (start, ends, delta) u =
    or (map (\q -> elem q ends) (hat delta start u))

-- This is just convenient for using `hat' in ghci.
getTransitions :: Automaton st sy -> [(st,sy,st)]
getTransitions (start, ends, delta) = delta

