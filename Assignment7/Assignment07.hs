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

-- B Semiring
instance Semiring Int where
    gconj x y = x * y
    gdisj x y = x + y
    gtrue = 1
    gfalse = 0

-- B
boolToCount :: GenericCFG nt t Bool -> GenericCFG nt t Int
boolToCount (starts, rules) =
    ([(c, case p of {False -> gfalse; True -> gtrue}) | (c,p) <- starts],
    [(r, case p of {False -> gfalse; True -> gtrue}) | (r,p) <- rules])

-- C Semiring
instance Semiring [Double] where
    gconj x y = [ x' * y' | x' <- x, y' <- y]
    gdisj x y = x ++ y
    gtrue = [1.0]
    gfalse = []

-- C
probToProbList :: GenericCFG nt t Double -> GenericCFG nt t [Double]
probToProbList (starts, rules) =
    ([(c, [p]) | (c,p) <- starts],
    [(r, [p]) | (r,p) <- rules])

-- D Semiring
instance Semiring [[RewriteRule nt t]] where
    gconj x y = [ x' ++ y' | x' <- x, y' <- y]
    gdisj x y = x ++ y
    gtrue = [[]]
    gfalse = []

-- D
boolToDerivList :: GenericCFG nt t Bool -> GenericCFG nt t [[RewriteRule nt t]]
boolToDerivList (starts, rules) =
    ([(c, case p of {True -> gtrue; False -> gfalse}) | (c,p) <- starts],
    [(r, case p of {True -> [[r]]; False -> gfalse}) | (r,p) <- rules])

-- E Semiring
instance Semiring [(Double, [RewriteRule nt t])]  where
    gconj x y = [ conjDoubleAndRuleList x' y' | x' <- x, y' <- y]
    gdisj x y = x ++ y
    gtrue = [(1.0, [])]
    gfalse = []

-- E Helper
conjDoubleAndRuleList ::(Double, [RewriteRule nt t]) -> (Double, [RewriteRule nt t]) -> (Double, [RewriteRule nt t])
conjDoubleAndRuleList x y = 
    let (doubleX, listOfRulesX) = x in
        let (doubleY, listOfRulesY) = y in
            (doubleX * doubleY, listOfRulesX ++ listOfRulesY)

-- E
probToProbDerivList :: GenericCFG nt t Double -> GenericCFG nt t [(Double, [RewriteRule nt t])]
probToProbDerivList (starts, rules) =
    ([(c, [(p, [])]) | (c,p) <- starts],
    [(r, [(p, [r])]) | (r,p) <- rules])

-- F Semiring
instance Semiring Cost  where
    gconj x y = case x of
                    Inf -> Inf            
                    TheInt x' -> case y of
                                    TheInt y' -> TheInt (x' + y')
                                    Inf -> Inf
    gdisj x y =   case x of
                    Inf -> y
                    TheInt x' -> case y of
                                    Inf -> x
                                    TheInt y' -> case (x' < y') of
                                                    True -> x
                                                    False -> y
    gtrue = TheInt 0
    gfalse = Inf

-- F & G Helper
boolAndRuleToCost :: (Eq nt) => Bool -> RewriteRule nt t -> Cost
boolAndRuleToCost p r = case p of {False -> gfalse; True ->
                    case (isCompoundRule r) of
                        True -> TheInt 0
                        False -> case (isAdjunctRule r) of
                            True -> TheInt 1
                            False -> TheInt 3}

-- F
boolToLowestCost :: (Eq nt) => GenericCFG nt t Bool -> GenericCFG nt t Cost
boolToLowestCost (starts, rules) =
    let newStarts = map (\(c, p) -> (c, case p of {False -> gfalse; True -> gtrue})) starts in
    let newRules = map (\(r, p) -> (r, boolAndRuleToCost p r)) rules in
        (newStarts, newRules)

-- G Semiring
instance Semiring (Cost, [[RewriteRule nt t]])  where
    gconj x y = let (costX, listOfRulesX) = x in
                    let (costY, listOfRulesY) = y in
                        (gconj costX costY, gconj listOfRulesX listOfRulesY)
    gdisj x y = let (costX, listOfRulesX) = x in
                    let (costY, listOfRulesY) = y in
                    case costX of
                        Inf -> case costY of
                            Inf -> (gdisj costX costY, gdisj listOfRulesX listOfRulesY)
                            TheInt intY -> y
                        TheInt intX -> case costY of
                            Inf -> x
                            TheInt intY -> case (intX == intY) of
                                True -> (gdisj costX costY, gdisj listOfRulesX listOfRulesY)
                                False -> case (intX < intY) of
                                            True -> x
                                            False -> y     
    gtrue = (gtrue, gtrue)
    gfalse = (gfalse, gfalse)

-- G Helper
boolAndRuleToListOfRuleLists :: Bool -> RewriteRule nt t -> [[RewriteRule nt t]]
boolAndRuleToListOfRuleLists p r = case p of {True -> [[r]]; False -> gfalse}

-- G
boolToLowestCostDerivs :: (Eq nt) => GenericCFG nt t Bool -> GenericCFG nt t (Cost, [[RewriteRule nt t]])
boolToLowestCostDerivs (starts, rules) =
    ([(c, case p of {True -> gtrue; False -> gfalse}) | (c,p) <- starts],
    [(r, (boolAndRuleToCost p r, boolAndRuleToListOfRuleLists p r)) | (r,p) <- rules])