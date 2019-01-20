module Assignment02 where

-- Make some standard list functions unavailable. :-)
import Prelude hiding (length, map, filter, take, drop, head, tail, last, init, reverse, (!!))

import Recursion

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

------------------------------------------------------------------

-- [1] Recursive functions on the Numb type

-- computes the product of two numbers
mult :: Numb -> Numb -> Numb
mult = \m -> case m of
              Z -> \n -> Z -- anything times zero is zero
              S m' -> \n -> case n of
                              Z -> Z -- anything times zero is zero
                              S n' -> add m (mult n' m)

-- computes the sum of all the numbers less than or equal to the given number
sumUpTo :: Numb -> Numb
sumUpTo = \n -> case n of
                  Z -> Z
                  S n' -> add n (sumUpTo n')

-- returns True if the two numbers given are equal, and False otherwise
equal :: Numb -> Numb -> Bool
equal = \m -> case m of
              Z -> \n -> case n of {Z -> True; x -> False}
              S m' -> \n -> case n of
                              Z -> False
                              S n' -> equal m' n'

------------------------------------------------------------------

-- [2] Recursive functions on lists

-- returns the number of elements in the given list for which the given argument returns True
count :: (a -> Bool) -> [a] -> Numb
count f l = case l of
            [] -> Z
            x : rest -> case (f x) of {True -> add one (count f rest); False -> count f rest}

-- returns a list containing the given element the given number of times
listOf :: Numb -> a -> [a]
listOf n element = case n of
                    Z -> []
                    S n' -> element : listOf n' element

-- addToEnd x l returns a list which is like l but has an additional occurrence of x at the end
addToEnd :: a -> [a] -> [a]
addToEnd element list = case list of
                        [] -> element : []
                        x : rest -> x : (addToEnd element rest)

-- remove f l returns a list which is like l but with those elements for which f returns True removed
remove :: (a -> Bool) -> [a] -> [a]
remove f l = case l of
              [] -> []
              x : rest -> case (f x) of
                            True -> remove f rest
                            False -> x : (remove f rest)

-- prefix n list returns the list containing the first n elements of list; or, if n is greater than the length of list, returns list as it is
prefix :: Numb -> [a] -> [a]
prefix n l = case n of
              Z -> []
              S n' -> case l of
                        [] -> []
                        x : rest -> x : prefix n' rest

------------------------------------------------------------------

-- [3] Recursive functions on the RegExp type

-- returns the number of occurrences of the star operator in the given regular expression
countStars :: RegExp -> Numb
countStars r = case r of
                 Lit x -> Z
                 Alt r1 r2 -> add (countStars r1) (countStars r2)
                 Concat r1 r2 -> add (countStars r1) (countStars r2)
                 Star r -> add one (countStars r)
                 ZeroRE -> Z
                 OneRE -> Z

-- returns the length of the longest root-to-leaf sequence of nodes in this tree for the given regular expression
depth :: RegExp -> Numb
depth r = case r of
            Lit x -> S Z
            Alt r1 r2 -> add one (bigger (depth r1) (depth r2))
            Concat r1 r2 -> add one (bigger (depth r1) (depth r2))
            Star r -> add one (depth r)
            ZeroRE -> S Z
            OneRE -> S Z

-- returns the string representing the given regular expression in the notation we used in class, with some obvious simplifications
reToString :: RegExp -> [Char]
reToString r = case r of
                Lit x -> [x]
                Alt r1 r2 -> "(" ++ (reToString r1) ++ "|" ++ (reToString r2) ++ ")"
                Concat r1 r2 -> "(" ++ (reToString r1) ++ "." ++ (reToString r2) ++ ")"
                Star r -> (reToString r) ++ "*"
                ZeroRE -> "0"
                OneRE ->  "1"