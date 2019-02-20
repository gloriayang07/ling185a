module LeftmostCheck where

import CFG

-- 'leftmostCheckHelper x rulelist' tries to find some sequence of rules at the beginning 
-- of rulelist that counts as a leftmost derivation with root category x. If there is no 
-- such sequence of rules, then this function returns Nothing. If there is such a sequence 
-- of rules, then this function returns a Just value with a list containing all of the 
-- remaining, unused rules from rulelist.
leftmostCheckHelper :: (Eq nt, Eq t) => nt -> [RewriteRule nt t] -> Maybe [RewriteRule nt t]
leftmostCheckHelper x [] = Nothing
leftmostCheckHelper x (r:rs) =
    case r of
    TermRule nt sym -> if x == nt then Just rs else Nothing
    NontermRule nt (nt1,nt2) ->
        if x == nt then
            case (leftmostCheckHelper nt1 rs) of
            Nothing -> Nothing
            Just rest1 -> leftmostCheckHelper nt2 rest1
        else
            Nothing

leftmostCheck :: (Eq nt, Eq t) => [RewriteRule nt t] -> Bool
leftmostCheck [] = False
leftmostCheck (r:rs) =
    -- Grab the nonterminal on the left-hand side of the first rule
    let nt = case r of {TermRule x y -> x; NontermRule x (y,z) -> x} in
    -- Now check to see if a derivation of that nonterminal uses up 
    -- the entire list of rules we've been given.
    case leftmostCheckHelper nt (r:rs) of
    Nothing -> False
    Just xs -> xs == []

