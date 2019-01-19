module Recursion where

-- data Result = Draw | Win Shape deriving Show

-- data Bool = True | False deriving Show

data Form = T | F | Neg Form | Cnj Form Form | Dsj Form Form deriving Show

f1 :: Form
f1 = Dsj (Neg F) T

f2 :: Form
f2 = Dsj (Neg T) (Cnj F T)

removeNegs :: Form -> Form
removeNegs = \f -> case f of
                   T -> T
                   F -> F
                   Neg phi -> removeNegs phi
                   Cnj phi psi -> Cnj (removeNegs phi) (removeNegs psi)
                   Dsj phi psi -> Dsj (removeNegs phi) (removeNegs psi)

-- -- The definition of '&&' under the hood looks like this.
-- x && y = case x of {True -> y; False -> False}

denotation :: Form -> Bool
denotation = \f -> case f of
                   T -> True
                   F -> False
                   Neg phi -> case (denotation phi) of {False -> True; True -> False}
                   Cnj phi psi -> case (denotation phi) of
                                  False -> False
                                  True -> denotation psi
                   Dsj phi psi -> case (denotation phi) of
                                  False -> denotation psi
                                  True -> True

--------------------------------------------

data Numb = Z | S Numb deriving Show

one = S Z
two = S one
three = S two
four = S three
five = S four

isOne :: Numb -> Bool
isOne = \n -> case n of
              Z -> False
              S n' -> case n' of {Z -> True; S n'' -> False}

double :: Numb -> Numb
double = \n -> case n of
               Z -> Z
               S n' -> S (S (double n'))

add :: Numb -> (Numb -> Numb)
add = \m -> case m of
            Z -> \n -> n
            S m' -> \n -> (add m') (S n)   -- S (add m' n)

dbl :: Int -> Int
dbl = \n -> if (n == 0) then 0 else 2 + (dbl (n-1))

isOdd :: Numb -> Bool
isOdd = \n -> case n of
              Z -> False
              S n' -> not (isOdd n')

bigger :: Numb -> Numb -> Numb
bigger = \m -> \n -> case m of
                     Z -> n
                     S m' -> case n of {Z -> m; S n' -> S (bigger m' n')}

difference :: Numb -> Numb -> Numb
difference = \m -> \n -> case m of
                         Z -> n
                         S m' -> case n of {Z -> m; S n' -> difference m' n'}

--------------------------

data IntList = Empty | NonEmpty Int IntList deriving Show

mylist = NonEmpty 5 (NonEmpty 7 (NonEmpty 2 Empty))
mylist2 = 5 : (7 : (2 : []))

total :: IntList -> Int
total = \l -> case l of
              Empty -> 0
              NonEmpty x rest -> x + total rest

total2 :: [Int] -> Int
total2 = \l -> case l of
               [] -> 0
               x : rest -> x + total2 rest

contains :: (a -> Bool) -> [a] -> Bool
contains f l = case l of
               [] -> False
               x : rest -> case (f x) of {True -> True; False -> contains f rest}

----------------------------------

-- This type's definition corresponds directly to the definition 
-- in (16) on this week's handout, taking the type Char as our alphabet sigma.
data RegExp = Lit Char
            | Alt RegExp RegExp
            | Concat RegExp RegExp
            | Star RegExp
            | ZeroRE
            | OneRE
            deriving Show

-- Some sample REs, from (17) on the handout
re17a = Alt (Lit 'a') (Lit 'b')
re17b = Concat re17a (Lit 'c')
re17c = Star re17b

-- This function corresponds directly to the definition 
-- in (18) on this week's handout.
denotationRE :: RegExp -> [[Char]]
denotationRE r = case r of
                 Lit x -> [ x:[] ]    -- a list containing a single string, which has length one
                 Alt r1 r2 -> denotationRE r1 ++ denotationRE r2
                 Concat r1 r2 -> map2 (\u -> \v -> u ++ v) (denotationRE r1) (denotationRE r2)
                 Star r -> [] : (map2 (\u -> \v -> u ++ v) (denotationRE r) (denotationRE (Star r)))
                 ZeroRE -> [  ]       -- a list containing zero strings
                 OneRE ->  [[]]       -- a list containing a single string, which has length zero

-- When I wrote map2 in a hurry in class, I accidentally used some slightly different 
-- syntax which we haven't talked about yet. 
-- Here's a version which does the same thing using the familiar syntax we've been seeing.
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f xs ys = case xs of
               [] -> []
               (x:rest) -> map (f x) ys ++ map2 f rest ys

