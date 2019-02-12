{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Prelude hiding (init)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring a) => a -> a -> a -> a
distrib_lhs x y z = gconj x (gdisj y z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
------------------------------------------------------------------

-- A
distrib_rhs :: (Semiring a) => a -> a -> a -> a
distrib_rhs x y z = gdisj (gconj x y) (gconj x z)

-- B
dotprod :: (Semiring v) => [v] -> [v] -> v
dotprod list1 list2 = case list1 of
                        [] -> gfalse
                        x:xs -> case list2 of
                          [] -> gfalse
                          y:ys -> gdisj (gconj x y) (dotprod xs ys)

-- C
expn :: (Semiring a) => a -> Numb -> a
expn val n = case n of
              Z -> gtrue
              S rest -> gconj val (expn val rest)

-- D
backward :: (Semiring a, Ord st, Ord sy) => GenericAutomaton st sy a -> [sy] -> st -> a
backward autom str q =
    let (starts, ends, trs) = autom in
    case str of
    []   -> fin autom q
    x:xs -> big_gdisj [gconj (tr autom q x q') (backward autom xs q') | q' <- allStates autom]

-- E
val :: (Semiring a, Ord st, Ord sy) => GenericAutomaton st sy a -> [sy] -> a
val autom str =
    let (starts, ends, trs) = autom in
    big_gdisj [gconj (init autom q) (backward autom str q) | q <- allStates autom]

-- F
addCost :: Cost -> Cost -> Cost
addCost cost1 cost2 =
  case cost1 of
    Inf -> Inf
    TheInt num1 -> case cost2 of
      Inf -> Inf
      TheInt num2 -> TheInt (num1 + num2)

-- G
minCost :: Cost -> Cost -> Cost
minCost cost1 cost2 =
  case cost1 of
    Inf -> cost2
    TheInt num1 -> case cost2 of
      Inf -> cost1
      TheInt num2 -> case (num1 < num2) of
        True -> cost1
        False -> cost2

-- H
instance Semiring Cost where
             gconj x y = addCost x y
             gdisj x y = minCost x y
             gtrue = TheInt 0
             gfalse = Inf

-- I
instance Semiring [String] where
             gconj u v = [ x ++ y | x <- u, y <- v]
             gdisj u v = concat [u, v]
             gtrue = [[]]
             gfalse = []

-- J
gfsa4 :: GenericAutomaton Int Char [String]
gfsa4 = (   [ (0, [[]]) ],
            [ (0, [[]]),
              (1, [[]]),
              (2, ["t"]) ],
            [ (0, 'n', ["n"], 0),
              (0, 't', ["t"], 0),
              (0, 'a', ["a"], 1),
              (1, 'n', ["n"], 0),
              (1, 'a', ["a"], 1),
              (1, 't', [[]], 2),
              (2, 'a', ["ta", "Ta"], 1),
              (2, 'n', ["tn"], 0),
              (2, 't', ["tt"], 0)  ])