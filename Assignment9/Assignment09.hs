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

