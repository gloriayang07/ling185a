{-# LANGUAGE FlexibleInstances #-}

module FinalProject where

import Assignment05

-- From Assignment #5
type GenericAutomaton st sy v = ([(st,v)], [(st,v)], [(st,sy,v,st)])

-- Potential to-do's
-- - Write a function that applys a list of rules and returns a list of strings, which represent strings at each step of the derivation
-- - Write a function that turns a list of rules into a FST
-- - Write a function that combines FSTs
-- - Model rule application using a different grammar
-- - Walk through real rules and demonstrate feeding, bleeding, counterfeeding, and couterbleeding
-- - Support other kinds of rules, such as ones bounded by words or those with only a pred or suc and not both
-- - Develop a notion of natural classes; ability to create rules with natural classes as environments (i.e. "before a vowel")
--              - Idea for this: the [pred] and [suc] should be a list of characters
-- - Do this same thing for morphological rules; demonstrate effects of morphology occuring before/after phonology

-- Of the form (isOptional, Target, Replacement, Predecessor, Successor) in Target -> Replacement / Predecessor, Successor
type Rule = (Bool, Char, Char, Char, Char)

-- Can have lists of characters as environment (i.e. 'apply this rule when beween vowels'), or Nothing
type NaturalClassRule = (Bool, Char, Char, Maybe [Char], Maybe [Char])

-- Some made up rules that would probably make a phonologist very angry
voicing = (False, 't', 'd', 'a', 'a')
raising = (True, 'a', 'u', 'p', 'd')

-- Two potential rule orderings, which will lead to different surface forms
ruleOrderOne = [voicing, raising]
ruleOrderTwo = [raising, voicing]

-- Some rules that accomodate natural classes
vowels = ['a', 'e', 'i', 'o', 'u']
voicingBetweenVowels = (False, 't', 'd', Just vowels, Just vowels)
voicingAfterAVowel = (False, 't', 'd', Just vowels, Nothing)
voicingBeforeAVowel = (False, 't', 'd', Nothing, Just vowels)

-- Quick little helper function for getting the alphabet, excluding a given character
alphabetWithout :: Char -> [Char]
alphabetWithout c = [sy | sy <- alphabet, sy /= c]

-- The alphabet
letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
wordBoundary = ['#']
alphabet = letters ++ wordBoundary

-- From Assignment #5
tappingFST :: GenericAutomaton Int Char [String]
tappingFST = (  [ (0, [[]]) ],
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

-- Take a rule and return a FST that applies the rule
ruleToFST :: Rule -> GenericAutomaton Int Char [String]
ruleToFST rule = 
    let (isOp, tar, rep, pred, suc) = rule in
            -- starts
        (   [ (0, [[]]) ],

            -- ends
            [ (0, [[]]),
              (1, [[]]),
              (2, [[tar]]) ],

            -- transitions
            [ (0, pred, [[pred]], 1),
              (1, pred, [[pred]], 1),
              (1, tar, [[]], 2),
              (2, suc, case isOp of {True-> [[rep, suc], [tar, suc]]; False -> [[rep, suc]]}, case (pred == suc) of {True -> 1; False -> 0}) ]
              ++ [ (0, el, [[el]], 0) | el <- alphabet, el /= pred ]
              ++ [ (1, el, [[el]], 0) | el <- alphabet, el /= pred && el /= tar ]
              ++ [ (2, el, [[tar, el]], 0) | el <- alphabet, el /= suc ] )

-- Given a string and a list of rules, applies those rules (in order) and returns a list of potential resulting strings
applyRuleList :: [Rule] -> [String] -> [String]
applyRuleList ruleList strList =
    case ruleList of
        [] -> strList
        (rule:rules) -> applyRuleList rules ( concat [ val (ruleToFST rule) str | str <- strList])

-- Take a natural class rule and return a FST that applies the rule
naturalClassRuleToFST :: NaturalClassRule -> GenericAutomaton Int Char [String]
naturalClassRuleToFST rule = 
    let (isOp, tar, rep, maybePreds, maybeSucs) = rule in
    let preds = case maybePreds of {Nothing -> alphabetWithout tar; Just l -> l} in
      let sucs = case maybeSucs of {Nothing -> alphabetWithout tar; Just l -> l} in
            -- starts
        (   [ (0, [[]]) ],

            -- ends
            [ (0, [[]]),
              (1, [[]]),
              (2, [[tar]]) ],

            -- transitions
            [ (1, tar, [[]], 2) ]
              ++ [(2, suc, case isOp of {True-> [[rep, suc], [tar, suc]]; False -> [[rep, suc]]}, case (elem suc preds) of {True -> 1; False -> 0}) | suc <- sucs]
              ++ [ (0, pred, [[pred]], 1) | pred <- preds ]
              ++ [ (1, pred, [[pred]], 1) | pred <- preds ]
              ++ [ (0, el, [[el]], 0) | el <- alphabet, not (elem el preds) ]
              ++ [ (1, el, [[el]], 0) | el <- alphabet, not (elem el preds) && el /= tar ]
              ++ [ (2, el, [[tar, el]], 0) | el <- alphabet, not (elem el sucs) ] )