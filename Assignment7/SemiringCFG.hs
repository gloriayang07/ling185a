module SemiringCFG where

import qualified Memoization as M

import Data.Set (fromList, toList)

-- A little trick for eliminating duplicates from a list
set :: (Ord a) => [a] -> [a]
set xs = toList (fromList xs)

-----------------------------------------------------------

class Semiring a where
    gconj :: a -> a -> a
    gdisj :: a -> a -> a
    gtrue :: a
    gfalse :: a

big_gconj :: Semiring a => [a] -> a
big_gconj list = case list of {[] -> gtrue; (x:xs) -> gconj x (big_gconj xs)}
big_gdisj :: Semiring a => [a] -> a
big_gdisj list = case list of {[] -> gfalse; (x:xs) -> gdisj x (big_gdisj xs)}

instance Semiring Bool where
    gconj x y = x && y
    gdisj x y = x || y
    gtrue = True
    gfalse = False

instance Semiring Double where
    gconj x y = x * y
    gdisj x y = x + y
    gtrue = 1.0
    gfalse = 0.0

-----------------------------------------------------------

-- Each potential starting state and each rule is paired with a 
-- value (maybe a boolean, maybe a probability, ...)
type GenericCFG nt t a = ([(nt,a)], [(RewriteRule nt t, a)])

data RewriteRule nt t = NontermRule nt (nt,nt) | TermRule nt t deriving (Show, Eq)

init :: (Semiring v, Ord nt) => GenericCFG nt t v -> nt -> v
init (starts, rules) c = big_gdisj [v | (c',v) <- starts, c == c']

trw :: (Semiring v, Ord nt, Ord t) => GenericCFG nt t v -> nt -> t -> v
trw (starts, rules) c x = big_gdisj [v | (TermRule c' x', v) <- rules, (c,x) == (c',x')]

ntrw :: (Semiring v, Ord nt, Ord t) => GenericCFG nt t v -> nt -> nt -> nt -> v
ntrw (starts, rules) c c1 c2 = big_gdisj [v | (NontermRule c' (c1',c2'), v) <- rules, (c,c1,c2) == (c',c1',c2')]

allNTs :: (Ord nt) => GenericCFG nt t v -> [nt]
allNTs (start, rules) = set [case r of {NontermRule x (y,z) -> x; TermRule x y -> x} | (r,v) <- rules]

-----------------------------------------------------------

data Cat = S | NP | VP | PP | V | P deriving (Show, Eq, Ord)

-- Sample grammar we used in class
cfg1 :: GenericCFG Cat String Bool
cfg1 = ([(S,True)], 
         [(r,True) | r <- [NontermRule S (NP,VP), 
                           NontermRule NP (NP,NP), 
                           NontermRule NP (NP,PP), 
                           NontermRule VP (V,NP), 
                           NontermRule VP (VP,PP), 
                           NontermRule PP (P,NP), 
                           TermRule NP "fish", TermRule NP "sheep", TermRule NP "deer", 
                           TermRule V "near", TermRule V "fish", 
                           TermRule P "near", TermRule P "with"
                          ]]
        )

-- Like cfg1 but with the additional possibility of using NP as starting nonterminal. 
-- Also includes some explicit `False' entries.
cfg2 :: GenericCFG Cat String Bool
cfg2 = let (starts1,rules1) = cfg1 in
       let newStarts = starts1 ++ [(NP,True), (VP,False)] in
       let newRules = rules1 ++ [(NontermRule S (VP,VP), False), (TermRule V "sheep", False)] in
       (newStarts, newRules)

-- Probabilistic version of cfg1
cfg3 :: GenericCFG Cat String Double
cfg3 = ([(S,1.0)], 
         [(NontermRule S (NP,VP), 1.0), 
          (NontermRule NP (NP,NP), 0.1), 
          (NontermRule NP (NP,PP), 0.2), 
          (NontermRule VP (V,NP), 0.7), 
          (NontermRule VP (VP,PP), 0.3), 
          (NontermRule PP (P,NP), 1.0), 
          (TermRule NP "fish", 0.2), (TermRule NP "sheep", 0.4), (TermRule NP "deer", 0.1), 
          (TermRule V "near", 0.4), (TermRule V "fish", 0.6), 
          (TermRule P "near", 0.5), (TermRule P "with", 0.5)
         ]
        )

cfg4 :: GenericCFG Cat String Double
cfg4 = ([(S,1.0)], 
         [(NontermRule S (NP,VP), 1.0), 
          (NontermRule PP (P,NP), 1.0), 
          (NontermRule VP (V,NP), 0.7), 
          (NontermRule VP (VP,PP), 0.3), 
          (NontermRule NP (NP,PP), 0.4), 
          (TermRule NP "astronomers", 0.1), (TermRule NP "ears", 0.18), (TermRule NP "saw", 0.04), 
          (TermRule NP "stars", 0.18), (TermRule NP "telescopes", 0.1), 
          (TermRule V "saw", 1.0), 
          (TermRule P "with", 1.0)
         ]
        )

-----------------------------------------------------------

-- We wrote this in class on Wed 2/20
insideCheck :: (Ord nt, Ord t) => GenericCFG nt t Bool -> [t] -> nt -> Bool
insideCheck cfg str c =
    case str of
    [] -> False
    (x:[]) -> trw cfg c x
    (x:(y:ys)) -> or [ntrw cfg c c1 c2 && insideCheck cfg (take k str) c1 && insideCheck cfg (drop k str) c2 |
                            c1 <- allNTs cfg, c2 <- allNTs cfg, k <- [1 .. length str - 1]]

-- Generalization of insideCheck to other semirings
inside :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> nt -> a
inside cfg str c =
    case str of
    [] -> gfalse
    (x:[]) -> trw cfg c x
    (x:(y:ys)) -> big_gdisj [big_gconj [ntrw cfg c c1 c2, inside cfg (take n str) c1, inside cfg (drop n str) c2] 
                                | n <- [1 .. length str - 1], c1 <- allNTs cfg, c2 <- allNTs cfg]

-- Faster version of inside
fastInside :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> nt -> a
fastInside cfg str c =
    M.memoize (\self -> \(str,c) ->
        case str of
        [] -> M.lift0 gfalse
        (x:[]) -> M.lift0 (trw cfg c x)
        (x:(y:ys)) -> M.liftList big_gdisj [M.liftList big_gconj [M.lift0 (ntrw cfg c l r), self (take n str, l), self (drop n str, r)]
                                            | n <- [1 .. length str - 1], l <- allNTs cfg, r <- allNTs cfg]
    ) (str,c)

