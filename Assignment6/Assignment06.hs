module Assignment06 where

import CFG

lhs :: RewriteRule nt t -> nt
lhs r = case r of
        NontermRule x (y,z) -> x
        TermRule x y -> x

rhs :: RewriteRule nt t -> [Symbol nt t]
rhs r = case r of
        NontermRule x (y,z) -> [NT y, NT z]
        TermRule x y -> [T y]

--------------------------------
---- Natural numbers

data Numb = Z | S Numb deriving Show

(one, two, three, four, five, six, seven, eight, nine, ten)
    = (S Z, S one, S two, S three, S four, S five, S six, S seven, S eight, S nine)   -- a bit sneaky

add :: Numb -> Numb -> Numb
add n m = case n of {Z -> m; S n' -> add n' (S m)}

--------------------------------
---- Two more example grammars

-- Equivalent to (3) on the handout
cfg_anbn :: CFG Int Char
cfg_anbn = (0, 
            [NontermRule 0 (10, 1), 
             NontermRule 1 (0, 11), 
             NontermRule 0 (10,11), 
             TermRule 10 'a', 
             TermRule 11 'b'
            ]
           )

-- Equivalent to (11) on the handout
data Cat = NP | X | CNJ deriving (Show, Eq)
cfg_ambiguity :: CFG Cat String
cfg_ambiguity = (NP, 
                 [NontermRule NP (NP, X), 
                  NontermRule X (CNJ, NP), 
                  TermRule NP "apples", 
                  TermRule NP "bananas", 
                  TermRule NP "oranges", 
                  TermRule CNJ "and", 
                  TermRule CNJ "or"
                 ]
                )

--------------------------------
---- Trees to represent analyses

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

root :: Tree nt t -> nt
root tree =
    case tree of
    Leaf x y -> x
    NonLeaf x t1 t2 -> x

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
------------------------------------------------------------------

-- A | helper function: returns whether a list of symbols contains a non-terminal symbol
listContainsNT :: [Symbol nt t] -> Bool
listContainsNT list = case list of
                [] -> False
                (x:xs) -> case x of {T val -> False; NT val -> True} || listContainsNT xs

-- A
terminalsOnly :: [Symbol nt t] -> Maybe [t]
terminalsOnly list = case listContainsNT list of
    True -> Nothing
    False -> Just (map (\sy -> case sy of {T val -> val}) list)

-- B
yield :: Tree nt t -> [t]
yield tree = case tree of
    Leaf nonterm term -> [term]
    NonLeaf nonterm leftTree rightTree -> (yield leftTree) ++ (yield rightTree)

-- C
treeToRuleList :: Tree nt t -> [RewriteRule nt t]
treeToRuleList tree = case tree of
    Leaf nonterm term -> [TermRule nonterm term]
    NonLeaf nonterm leftTree rightTree -> [NontermRule nonterm (root leftTree, root rightTree)]
        ++ treeToRuleList leftTree
        ++ treeToRuleList rightTree

-- D
ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree list = case leftSideOfFirstRuleInList list of
                        Nothing -> Nothing
                        Just startingVal -> ruleListToTreeHelper list startingVal

-- D | helper function
-- Gets starting element
leftSideOfFirstRuleInList :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (nt)
leftSideOfFirstRuleInList list = case list of
    [] -> Nothing
    (x:xs) -> case x of
        TermRule mother child -> Just mother
        NontermRule mother (leftChild,rightChild) -> Just mother

-- D | helper function
-- This almost works!
ruleListToTreeHelper list node = case list of
    [] -> Nothing
    (rule:rest) -> case rule of
        TermRule mother child ->
            if mother == node then Just (Leaf mother child)
            else Nothing
        NontermRule mother (leftChild,rightChild) ->            
            let maybeLeftTree = ruleListToTreeHelper rest leftChild in
            let maybeRightTree = ruleListToTreeHelper rest rightChild in
            case maybeLeftTree of
                Nothing -> Nothing
                Just treeL -> case maybeRightTree of
                    Nothing -> Nothing
                    Just treeR -> case mother == node of
                        False -> Nothing
                        True -> Just (NonLeaf mother treeL treeR)

-- E | helper function: get all of a tree's terminal symbols
treeToTerminals :: Tree nt t -> [Symbol nt t]
treeToTerminals tree = case tree of
    Leaf nonterm term -> [T term]
    NonLeaf nonterm leftTree rightTree -> treeToTerminals leftTree ++ treeToTerminals rightTree

-- E
treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation tree = case tree of
    Leaf nonterm term -> [[NT nonterm], [T term]]
    NonLeaf nonterm leftTree rightTree ->
        let leftResult = init (treeToDerivation leftTree) in -- init excludes the last element
        let rightResult = treeToDerivation rightTree in
        [[NT nonterm]]
        ++ [ left ++ right | left <- leftResult, right <- [[NT (root rightTree)]]]
        ++ [ left ++ right | left <- [(treeToTerminals leftTree)], right <- rightResult]

-- F
splitAtLeftmost :: (Eq nt, Eq t) => [Symbol nt t] -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost list =
    case list of
    [] -> Nothing
    (x:xs) ->
        case x of
            NT value -> Just ([], value, xs)
            _ -> case (splitAtLeftmost xs) of
                Nothing -> Nothing
                Just (ys, qs ,zs) -> Just (x:ys, qs ,zs)

-- G
rewriteLeftmost :: (Eq nt, Eq t) => [RewriteRule nt t] -> [Symbol nt t] -> [[Symbol nt t]]
rewriteLeftmost ruleList symbolList =
    let splitList = (splitAtLeftmost symbolList) in
        case splitList of
            Nothing -> []
            Just (x, y, z) -> let Just (left, nonterm, right) = splitList in
                case ruleList of
                    [] -> []
                    (rule:otherRules) -> let recursiveCall = rewriteLeftmost otherRules symbolList in
                        case rule of
                            NontermRule x (y,z) -> if x == nonterm then [left ++ [NT y] ++ [NT z] ++ right] ++ recursiveCall
                                                    else recursiveCall
                            TermRule x y -> if x == nonterm then [left ++ [T y] ++ right] ++ recursiveCall
                                                    else recursiveCall

-- H
derivableFrom :: (Eq nt, Eq t) => [Symbol nt t] -> [RewriteRule nt t] -> Numb -> [[t]]
derivableFrom symbolList ruleList n = removeTs (derivableFromHelper symbolList ruleList n)

-- H | helper function
derivableFromHelper :: (Eq nt, Eq t) => [Symbol nt t] -> [RewriteRule nt t] -> Numb -> [[Symbol nt t]]
derivableFromHelper symbolList ruleList n = case n of
    Z -> []
    S n' -> let potentialStrings = rewriteLeftmost ruleList symbolList in
            let onesWithAtLeastOneNT = filter (\x -> listContainsNT x) potentialStrings in
            let onesWithTerminalsOnly = filter (\x -> case (listContainsNT x) of {True -> False; False -> True}) potentialStrings in
            concat (map (\x -> derivableFromHelper x ruleList n') onesWithAtLeastOneNT) ++ onesWithTerminalsOnly

-- H | helper function
-- turns things like [[T "John", T "ran"],[T "Mary"]] into [["John", "ran"],["Mary"]]
removeTs :: (Eq nt, Eq t) => [[Symbol nt t]] -> [[t]]
removeTs list = map (\listOfSymbols -> (map (\sy -> case sy of {T val -> val}) listOfSymbols)) list