{-# LANGUAGE FlexibleInstances #-}

module Assignment07 where

import Prelude hiding (init)

import SemiringCFG

-------------------------------------
-- probToBool is described at the 
-- beginning of section 2

probToBool :: GenericCFG nt t Double -> GenericCFG nt t Bool
probToBool (starts, rules) =
    ([(c, p > 0) | (c,p) <- starts], [(r, p > 0) | (r,p) <- rules])

--------------------------------
-- setup for questions F and G

data Cost = TheInt Int | Inf deriving (Show, Eq)

isCompoundRule :: (Eq nt) => RewriteRule nt t -> Bool
isCompoundRule (NontermRule c (c1,c2)) = c == c1 && c == c2
isCompoundRule (TermRule c x) = False

isAdjunctRule :: (Eq nt) => RewriteRule nt t -> Bool
isAdjunctRule (NontermRule c (c1,c2)) = (c == c1 || c == c2) && (c1 /= c2)
isAdjunctRule (TermRule c x) = False

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
------------------------------------------------------------------

-- A
val :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> a
val cfg ts = big_gdisj [ gconj (init cfg x) (fastInside cfg ts x) | x <- (allNTs cfg)]

-- B
-- boolToCount :: GenericCFG nt t Bool -> GenericCFG nt t Int
-- boolToCount (starts, rules) = TBD
