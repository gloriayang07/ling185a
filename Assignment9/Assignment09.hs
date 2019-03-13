module Assignment09 where

import TreeGrammars

-----------------------------------------------------------------
-- Section 2.1: The basic pattern

data WhStatus = Wh | LicWh | WhOK deriving (Eq,Ord,Show)

fsta_wh1 :: Automaton WhStatus String Gam1 Gam2
fsta_wh1 = (-- ending states
            [WhOK],
            -- nullary transitions
            [(s,WhOK) | s <- ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]] ++
            [(s,Wh) | s <- ["WH"]] ++
            [("Q",LicWh)],
            -- unary transitions
            [],
            -- binary transitions
            [(Wh, Wh,     Merge, Wh),
             (Wh, WhOK,   Merge, Wh),
             (WhOK, Wh,   Merge, Wh),
             (WhOK, WhOK, Merge, WhOK),
             (LicWh, Wh,  Merge, WhOK),
             (Wh, LicWh,  Merge, WhOK)]
            )

-- (1a)/(2a) `C John ate an apple'
tree_1a :: Tree String Gam1 Gam2
tree_1a = Binary Merge (Leaf "C") 
                       (Binary Merge (Leaf "John")
                                     (Binary Merge (Leaf "ate") (Binary Merge (Leaf "an") (Leaf "apple")))
                       )

-- (1b)/(2b) `Q John ate what'
tree_1b :: Tree String Gam1 Gam2
tree_1b = Binary Merge (Leaf "Q") 
                       (Binary Merge (Leaf "John")
                                     (Binary Merge (Leaf "ate") (Leaf "WH"))
                       )

-- (3a) `Q John ate an apple'
tree_3a :: Tree String Gam1 Gam2
tree_3a = Binary Merge (Leaf "Q") 
                       (Binary Merge (Leaf "John")
                                     (Binary Merge (Leaf "ate") (Binary Merge (Leaf "an") (Leaf "apple")))
                       )

-- (3b) `C John ate what'
tree_3b :: Tree String Gam1 Gam2
tree_3b = Binary Merge (Leaf "C") 
                       (Binary Merge (Leaf "John")
                                     (Binary Merge (Leaf "ate") (Leaf "WH"))
                       )

-----------------------------------------------------------------
-- Section 2.2: Island effects

data MrgAdj = Mrg | Adj deriving (Show,Ord,Eq)

tree_13 :: Tree String Gam1 MrgAdj
tree_13 = Binary Mrg (Leaf "Q")
                     (Binary Mrg (Leaf "John")
                                 (Binary Adj (Leaf "laughed")
                                             (Binary Mrg (Leaf "because")
                                                         (Binary Mrg (Leaf "C")
                                                                     (Binary Mrg (Leaf "Mary")
                                                                                 (Binary Mrg (Binary Mrg (Leaf "bought") (Leaf "books"))
                                                                                             (Leaf "WH")
                                                                                 )
                                                                     )
                                                         )
                                             )
                                 )
                     )

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
------------------------------------------------------------------

-- #1 Boolean inside values
insideCheck :: (Ord st, Eq sy0, Eq sy1, Eq sy2) => Automaton st sy0 sy1 sy2 -> Tree sy0 sy1 sy2 -> st -> Bool
insideCheck fsta tree st =
  let (ends, nullaries, unaries, binaries) = fsta in
  let allTheStates = allStates fsta in
    case tree of
      Leaf x ->                 elem (x, st) nullaries
      Unary x t -> or [         (insideCheck fsta t q') &&
                                (elem (q', x, st) unaries)
                                | q' <- allTheStates]
      Binary x t1 t2 -> or [    (insideCheck fsta t1 q1') &&
                                (insideCheck fsta t2 q2') &&
                                (elem (q1', q2', x, st) binaries) 
                                | q1' <- allTheStates, q2' <- allTheStates]

-- Possibly another (more efficient) way to do #1
-- insideCheck :: (Ord st, Eq sy0, Eq sy1, Eq sy2) => Automaton st sy0 sy1 sy2 -> Tree sy0 sy1 sy2 -> st -> Bool
-- insideCheck fsta tree st =
--   let (ends, nullaries, unaries, binaries) = fsta in
--     case tree of
--       Leaf x -> elem st [q | (y,q) <- nullaries, y == x]
--       Unary x t -> elem st [q | (q',y,q) <- unaries, y == x, (insideCheck fsta t q')]
--       Binary x t1 t2 -> elem st [q | (q1,q2,y,q) <- binaries, y == x, (insideCheck fsta t1 q1), 
--                                                                        (insideCheck fsta t2 q2)]

-- #2 Wh-in-situ dependencies
fsta_wh2 :: Automaton WhStatus String Gam1 MrgAdj
fsta_wh2 = (-- ending states
            [WhOK],
            -- nullary transitions
            [(s,WhOK) | s <- ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]] ++
            [(s,Wh) | s <- ["WH"]] ++
            [("Q",LicWh)],
            -- unary transitions
            [],
            -- binary transitions
            [(Wh, Wh,     Mrg, Wh),
             (Wh, WhOK,   Mrg, Wh),
             (WhOK, Wh,   Mrg, Wh),
             (WhOK, WhOK, Mrg, WhOK),
             (LicWh, Wh,  Mrg, WhOK),
             (Wh, LicWh,  Mrg, WhOK),
             
             (WhOK, WhOK, Adj, WhOK),
             (LicWh, Wh,  Adj, WhOK),
             (Wh, LicWh,  Adj, WhOK)
             ])