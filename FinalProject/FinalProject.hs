{-# LANGUAGE FlexibleInstances #-}

module FinalProject where

import Assignment05

-- From Assignment05
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

-- Rules
voicing = (False, 't', 'd', 'a', 'a')
raising = (False, 'a', 'u', 'p', 'd')

ruleOrderOne = [voicing, raising]
ruleOrderTwo = [raising, voicing]

-- The alphabet we're working with
phonemes = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
allophones = ['A', 'T', 'B']
alphabet = phonemes ++ allophones

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