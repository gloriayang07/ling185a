module CFG where

-- a starting nonterminal and a list of rewrite rules
type CFG nt t = (nt, [RewriteRule nt t])

-- enforcing Chomsky normal form
data RewriteRule nt t = NontermRule nt (nt,nt) | TermRule nt t deriving (Show, Eq)

data Symbol nt t = NT nt | T t deriving (Show, Eq)
-- e.g. NT 3 :: Symbol Int Char
-- e.g. T 'a' :: Symbol Int Char

cfg1 :: CFG String String
cfg1 = ("S", 
        [NontermRule "S" ("NP","VP"), 
         NontermRule "NP" ("D","N"), 
         NontermRule "VP" ("V","NP"), 
         TermRule "NP" "John",  TermRule "NP" "Mary", 
         TermRule "D" "the",    TermRule "D" "a", 
         TermRule "N" "cat",    TermRule "N" "dog", 
         TermRule "V" "saw",    TermRule "V" "likes"
        ]
       )

-- reminder:
-- data Maybe a = Nothing | Just a

test :: Maybe [Symbol String String]
test = rewrite (NontermRule "NP" ("D","N")) [NT "S", T "mary", NT "NP", T "the"]

-- This (arbitrarily) splits at the leftmost occurrence of the given nonterminal.
-- This is not necessary the same as splitting at the leftmost nonterminal!
splitAtNT :: (Eq nt, Eq t) => nt -> [Symbol nt t] -> Maybe ([Symbol nt t], [Symbol nt t])
splitAtNT key list =
    case list of
    [] -> Nothing
    (x:xs) ->
        if x == NT key then
            Just ([], xs)
        else
            case (splitAtNT key xs) of
            Nothing -> Nothing
            Just (ys, zs) -> Just (x:ys, zs)

-- rewrite (A -> beta) (alpha A gamma) ==> alpha beta gamma
-- rewrite (NP -> D N) [S, mary, NP, the] ==> Just [S, mary, D, N, the]
-- rewrite (NP -> D N) [S, mary, VP, the] ==> Nothing
rewrite :: (Eq nt, Eq t) => RewriteRule nt t -> [Symbol nt t] -> Maybe [Symbol nt t]
rewrite r list =
    let (a, beta) = case r of
                    NontermRule x (y,z) -> (x, [NT y, NT z])
                    TermRule x y -> (x, [T y])
    in
    case splitAtNT a list of
    Nothing -> Nothing
    Just (alpha,gamma) -> Just (alpha ++ beta ++ gamma)

