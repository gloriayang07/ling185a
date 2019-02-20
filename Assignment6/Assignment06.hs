module Assignment06 where

import CFG

import LeftmostCheck -- temp

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

-- A
-- helper function: returns whether a list of symbols contains a non-terminal symbol
listContainsNT :: [Symbol nt t] -> Bool
listContainsNT list = case list of
                [] -> False
                (x:xs) -> case x of {T val -> False; NT val -> True} || listContainsNT xs

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
-- ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
-- ruleListToTree list startingSymbol = case list of
--     [] -> Nothing
--     (rule:rest) -> case rule of
--         TermRule mother child -> Just (Leaf mother child)
--         NontermRule mother (leftChild,rightChild) ->
--             let leftSubtree = ruleListToTree rest

ruleListToTree list node = case list of
    [] -> Nothing
    (rule:rest) -> case rule of
        TermRule mother child ->
            if mother == node then Just (Leaf mother child)
            else Nothing
        NontermRule mother (leftChild,rightChild) ->            
            let maybeLeftTree = ruleListToTree rest leftChild in
            let maybeRightTree = ruleListToTree rest rightChild in
            case maybeLeftTree of
                Nothing -> Nothing
                Just treeL -> case maybeRightTree of
                    Nothing -> Nothing
                    Just treeR -> case mother == node of
                        False -> Nothing
                        True -> Just (NonLeaf mother treeL treeR)

-- getRuleForNode :: (Eq nt, Eq t) => [RewriteRule nt t] -> nt -> Maybe (RewriteRule nt t)
-- getRuleForNode list nonterm = case list of
--     [] -> Nothing
--     (rule:rest) -> case rule of
--         TermRule mother child ->
--             if mother == nonterm then Just rule
--             else getRuleForNode rest nonterm
--         NontermRule mother (leftChild,rightChild) ->
--             if mother == nonterm then Just rule
--             else getRuleForNode rest nonterm

-- test rule list
list1 = [NontermRule "S" ("NP","VP"), TermRule "NP" "Mary", TermRule "VP" "ran"]
list2 = [NontermRule "S" ("NP","VP"), TermRule "NP" "Mary", TermRule "VP" "ran", TermRule "NP" "Jeff"]
list3 = [TermRule "NP" "Mary"]

-- -- returns a list of rules in a list starting with the given nonterminal node
-- rulesStartingWithNode :: (Eq nt, Eq t) => nt -> [RewriteRule nt t] -> [RewriteRule nt t]
-- rulesStartingWithNode node list =
--     filter (\rule -> case rule of {
--         TermRule mother child -> mother == node;
--         NontermRule mother (leftChild,rightChild) -> mother == node;
--     }) list

-- -- returns an array of children of the given node
-- getChild node list =
--     let relevantRules = rulesStartingWithNode node list in
--     case relevantRules of
--     [] -> []
--     (x:xs) -> case x of
--         TermRule mother child -> [child]
--         NontermRule mother (leftChild,rightChild) -> [leftChild,rightChild]

-- getStartingNode list =
--     case list of
--     [] -> ""
--     (x:xs) -> case x of
--         TermRule mother child -> getStartingNode xs
--         NontermRule mother (leftChild,rightChild) -> mother

-- node is something like T "Mary" or NT "VP"
-- recursiveThing node =
--     case node of
--         T t -> Leaf TS_MOTHER t
--         NT nt -> NonLeaf (recursiveThing THING_NONLEAF_CAN_GO_TO_1) (recursiveThing THING_NONLEAF_CAN_GO_TO_1)

-- motherOfTerminalNode node list = 
--     case list of
--         [] -> []
--         (rule: rest) -> case rule of
--             TermRule mother child -> case child of {node -> mother; x -> motherOfTerminalNode node rest}
--             NontermRule mother (leftChild,rightChild) -> motherOfTerminalNode node rest

-- childrenOfNonterminalNode

        -- Rule List:   [NontermRule "S" ("NP","VP"), TermRule "NP" "Mary", TermRule "VP" "ran"]

-- Tree:        Just (NonLeaf "S" (Leaf "NP" "Mary") (Leaf "VP" "ran"))

-- E
-- helper function: get all of a tree's terminal symbols
treeToTerminals :: Tree nt t -> [Symbol nt t]
treeToTerminals tree = case tree of
    Leaf nonterm term -> [T term]
    NonLeaf nonterm leftTree rightTree -> treeToTerminals leftTree ++ treeToTerminals rightTree

treeToDerivation :: Tree nt t -> [[Symbol nt t]]
treeToDerivation tree = case tree of
    Leaf nonterm term -> [[NT nonterm], [T term]]
    NonLeaf nonterm leftTree rightTree ->
        let leftResult = init (treeToDerivation leftTree) in -- init excludes the last element
        let rightResult = treeToDerivation rightTree in
        [[NT nonterm]]
        ++ [ left ++ right | left <- leftResult, right <- [[NT (root rightTree)]]]
        ++ [ left ++ right | left <- [(treeToTerminals leftTree)], right <- rightResult]

-- Section notes on E:
-- might need a function to get the last thing in a list (i.e. last leftResult)
-- will need to do "let leftResult = treeToDerivation SOMETHING because I'll need to use left result a couple times"
-- list of steps to process the NP
-- stick VP to the end of the steps
-- use map or list comprehension to stick VP to the result of everything
-- need to calculate the treeToDerivation steps (maybe using let) before adding anything
-- "could be cleaner to write out two versions, one that takes Leaf and one nonleaf"
-- hint: you gotta use list comprehension
-- for left: we're appending the root of the right tree
-- for right: we're sticking terminalNodes of the left tree before the right tree

-- Example trees for E
tree1 = (NonLeaf "S" (Leaf "NP" "Mary") (Leaf "VP" "ran"))
tree2 = (NonLeaf "DP" (Leaf "D" "the") (Leaf "N" "dog"))
tree3 = (NonLeaf "VP" (Leaf "V" "ran") (Leaf "Adv" "quickly"))
tree4 = (NonLeaf "S" tree2 tree3)

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

-- let Just (left, nonterm, right) = splitAtLeftmost symbolList in
-- this worked before almost
-- -- G
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

-- -- H
-- derivableFrom :: (Eq nt, Eq t) => [Symbol nt t] -> [RewriteRule nt t] -> Numb -> [[t]]


-- Rule List
-- [   NontermRule "S" ("NP","VP"), 
--     NontermRule "NP" ("D","N"), 
--     NontermRule "VP" ("V","NP"), 
--     TermRule "NP" "John",  TermRule "NP" "Mary", 
--     TermRule "D" "the",    TermRule "D" "a", 
--     TermRule "N" "cat",    TermRule "N" "dog", 
--     TermRule "V" "saw",    TermRule "V" "likes"]