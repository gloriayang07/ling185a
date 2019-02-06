module FiniteStatePart2 where

import FiniteState

allStates :: (Ord st) => (st, [st], [(st,a,st)]) -> [st]
allStates (start, ends, delta) = set (start : ends ++ concat [[q1,q2] | (q1,x,q2) <- delta])

allLabels :: (Ord a) => (st, [st], [(st,a,st)]) -> [a]
allLabels (start, ends, delta) = set [x | (q1, x, q2) <- delta]

----------------------------------------------------------------------------
-- Intersection of FSAs

fsa_oddCs :: Automaton Bool SegmentCV
fsa_oddCs = (False, [True], [(False, C, True), (False, V, False), (True, C, False), (True, V, True)])

fsa_evenVs :: Automaton Bool SegmentCV
fsa_evenVs = (False, [False], [(False, C, False), (False, V, True), (True, C, True), (True, V, False)])

-- Corresponds to (4) on the handout
intersect :: (Ord st1, Ord st2, Ord sy) => Automaton st1 sy -> Automaton st2 sy -> Automaton (st1,st2) sy
intersect m m' =
    let (start, ends, delta) = m in
    let (start', ends', delta') = m' in
    let newStart = (start,start') in
    let newEnds = [(q,q') | q <- ends, q' <- ends'] in
    let newTransitions (q1,q1') x = [((q1,q1'), x, (q2,q2')) | q2 <- targets delta q1 x, q2' <- targets delta' q1' x] in
    let newDelta = concat [newTransitions (q1,q1') x | q1 <- allStates m, 
                                                       q1' <- allStates m', 
                                                       x <- set (allLabels m ++ allLabels m')] in
    (newStart, newEnds, newDelta)

----------------------------------------------------------------------------
-- FSAs with epsilon transitions

type EpsAutomaton st sy = (st, [st], [(st, Maybe sy, st)])

-- Corresponds to (6) on the class handout
efsa_handout6 :: EpsAutomaton Int Char
efsa_handout6 = (10, [20,30], [ (10, Just 'a', 10), (10, Nothing, 20),  (10, Nothing, 30), 
                                (20, Just 'b', 21), (21, Just 'b', 20), 
                                (30, Just 'b', 31), (31, Just 'b', 32), (32, Just 'b', 30) ] )

-- Corresponds to (7) on the class handout
efsa_handout7 :: EpsAutomaton Int Char
efsa_handout7 = (0, [2], [(0, Just 'a',  0), 
                          (0, Nothing, 1), 
                          (1, Just 'b',  1), 
                          (1, Nothing, 2), 
                          (2, Just 'c',  2)])

-- One more epsilon-FSA
efsa_xyz :: EpsAutomaton Int Char
efsa_xyz = (0, [1], [(0, Just 'x', 0), (0, Just 'y', 1), (0, Nothing, 1), (1, Just 'z', 1)])

-- Uses a predefined function 'until': 'until p f x' applies f to x repeatedly until p x is true
epsilonClosure :: (Ord st, Ord sy) => [(st, Maybe sy, st)] -> st -> [st]
epsilonClosure delta q =
    let update qs = qs ++ [q2 | q1 <- qs, q2 <- targets' delta q1 Nothing, not (elem q2 qs)] in
    until (\qs -> update qs == qs) update [q]

-- Corresponds to (9) on the handout
removeEpsilons :: (Ord st, Ord sy) => EpsAutomaton st sy -> Automaton st sy
removeEpsilons efsa =
    let (start, ends, delta) = efsa in
    let newTransitions q1 x = case x of
                              Nothing -> []
                              Just s -> [(q1,s,q3) | q2 <- epsilonClosure delta q1, 
                                                     q3 <- targets delta q2 (Just s)] 
    in
    let canReachEndState q = or (map (\q' -> elem q' ends) (epsilonClosure delta q)) in
    (start, filter canReachEndState (allStates efsa), 
            concat [newTransitions q x | q <- allStates efsa, x <- allLabels efsa])

