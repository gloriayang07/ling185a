module ProbFSA where

import Data.Set (fromList, toList)

-- A little trick for eliminating duplicates from a list
set :: (Ord a) => [a] -> [a]
set xs = toList (fromList xs)

------------------------------------------

-- Compare this type with the definition in (1) on the semirings handout. 
-- The type parameters st and sy encode the states and the alphabet, 
-- in the same way as we've been doing for plain FSAs. 
-- The three components of a ProbFSA encode the starting probabilities, 
-- ending probabilities and transition probabilities, respectively. 
-- The first component encodes the starting probabilities in the form of 
-- a list of state-probability pairs; we only include those states for 
-- which the probability is non-zero. Similarly, the second component 
-- only includes those states for which the ending probability is non-zero, 
-- and the third component only includes those transitions for which the 
-- probability is non-zero.
type ProbFSA st sy = ([(st,Float)], [(st,Float)], [(st,sy,Float,st)])

------------------------------------------

data WordSegState = Edge | Internal deriving (Show, Eq, Ord)

-- Corresponds to the PFSA used in examples in class. 
pfsa1 :: ProbFSA WordSegState Char
pfsa1 = 
    ([(Edge, 1.0)],
     [(Edge, 0.5)],
     [(Edge, 'a', 0.015, Edge),     (Internal, 'a', 0.042, Edge),
      (Edge, 'i', 0.015, Edge),     (Internal, 'e', 0.056, Edge),
                                    (Internal, 'i', 0.014, Edge),
                                    (Internal, 'n', 0.098, Edge),
                                    (Internal, 't', 0.084, Edge),
                                    (Internal, 's', 0.154, Edge), 
      (Edge, 'a', 0.103, Internal), (Internal, 'a', 0.085, Internal),
      (Edge, 'e', 0.029, Internal), (Internal, 'e', 0.149, Internal),
      (Edge, 'i', 0.088, Internal), (Internal, 'i', 0.149, Internal),
      (Edge, 'n', 0.029, Internal), (Internal, 'n', 0.085, Internal),
      (Edge, 't', 0.103, Internal), (Internal, 't', 0.021, Internal),
      (Edge, 's', 0.118, Internal), (Internal, 's', 0.064, Internal)
     ])

------------------------------------------

-- Just pulls out all the states mentioned anywhere in a ProbFSA
allStates :: (Ord st, Ord sy) => ProbFSA st sy -> [st]
allStates (starts, ends, trs) = set ([q | (q,p) <- starts] ++ 
                                     [q | (q,p) <- ends] ++ 
                                     [q1 | (q1,x,p,q2) <- trs] ++ 
                                     [q2 | (q1,x,p,q2) <- trs])

-- Finds starting probability of a state.
initProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> Float
initProb (starts, ends, trs) q = sum [p | (q',p) <- starts, q == q']
                                -- or equivalently:
                                --  sum (map (\(q',p) -> p) (filter (\(q',p) -> q == q') starts))

-- Finds ending probability of a state. Corresponds to fin_P on the handout.
finProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> Float
finProb (starts, ends, trs) q = sum [p | (q',p) <- ends, q == q']
                                -- or equivalently:
                                --  sum (map (\(q',p) -> p) (filter (\(q',p) -> q == q') ends))

-- Finds probability for particular state-symbol-state transition. Corresponds to tr_P on the handout.
trProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> sy -> st -> Float
trProb (starts, ends, trs) q1 x q2 = sum [p | (q,y,p,q') <- trs, (q,y,q') == (q1,x,q2)]
                                -- or equivalently:
                                --        sum (map (\(q,y,p,q') -> p)
                                --                 (filter (\(q,y,p,q') -> (q,y,q') == (q1,x,q2)) trs)
                                --        )

------------------------------------------

-- The backward probability of a string str and a state q is the answer to the question:
-- If I were to start out at state q, what's the probability of taking some sequence of 
-- transitions producing the string str and then ending?
-- The general definition of this is given in (10) on the semirings handout. 
-- This implementation uses the reformulation in (30) on that handout.
backwardProb :: (Ord st, Ord sy) => ProbFSA st sy -> [sy] -> st -> Float
backwardProb pfsa str q =
    let (starts, ends, trs) = pfsa in
    case str of
    []   -> finProb pfsa q
    x:xs -> sum [trProb pfsa q x q' * backwardProb pfsa xs q' | q' <- allStates pfsa]
        -- or equivalently:
        --      sum (map (\q' -> trProb pfsa q x q' * backwardProb pfsa xs q') (allStates pfsa))

-- Finds the total probability of a string.
-- See (13) on the semirings handout.
valProb :: (Ord st, Ord sy) => ProbFSA st sy -> [sy] -> Float
valProb pfsa str = sum [initProb pfsa q * backwardProb pfsa str q | q <- allStates pfsa]
            -- or equivalently:
            --      sum (map (\q -> initProb pfsa q * backwardProb pfsa str q) (allStates pfsa))

