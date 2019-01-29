module Assignment03 where

import FiniteState

data SegmentPKIU = P | K | I | U | WB deriving (Eq, Ord, Show)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
------------------------------------------------------------------

------------------------------------------------------------------

-- 1
fsa_asst1 :: Automaton Int Bool
fsa_asst1 = (54, [38], [(54, False, 54),
                        (54, True, 73),
                        (73, False, 73),
                        (73, True, 21),
                        (21, False, 21),
                        (21, True, 54),
                        (21, True, 38),
                        (38, False, 38)])

-- 2A
fsa_asst2a :: Automaton Int SegmentCV
fsa_asst2a = (0, [2], [(0, V, 0),
                       (0, C, 1),
                       (1, V, 1),
                       (1, C, 2),
                       (2, V, 2),
                       (2, C, 2)])

-- 2B
-- state is form (# of C's is even, # of V's is even)
-- 41 represents (True, True)
-- 42 represents (False, True)
-- 43 represents (True, False)
-- 44 represents (False, False)
fsa_asst2b :: Automaton Int SegmentCV
fsa_asst2b = (41, [42], [(41, C, 42),
                         (41, V, 43),
                         (42, C, 41),
                         (42, V, 44),
                         (43, C, 44),
                         (43, V, 41),
                         (44, C, 43),
                         (44, V, 42)])

-- 2C
fsa_asst2c :: Automaton Int SegmentCV
fsa_asst2c = (41, [44], [(41, C, 42),
                         (41, V, 42),
                         (42, C, 43),
                         (42, V, 43),
                         (43, C, 44),
                         (44, C, 44),
                         (44, V, 44)])

-- 2D
fsa_asst2d :: Automaton Int SegmentCV
fsa_asst2d = (41, [44], [(41, C, 41),
                         (41, V, 41),
                         (41, C, 42),
                         (42, C, 43),
                         (42, V, 43),
                         (43, C, 44),
                         (43, V, 44)])

-- 2E
fsa_asst2e :: Automaton Int SegmentPKIU
fsa_asst2e = (41, [41, 42, 43], [(41, P, 41),
                                 (41, K, 41),
                                 (41, WB, 41),
                                 (41, I, 42),
                                 (41, U, 43),
                                 (42, P, 42),
                                 (42, K, 42),
                                 (42, I, 42),
                                 (42, WB, 41),
                                 (43, P, 43),
                                 (43, K, 43),
                                 (43, U, 43),
                                 (43, WB, 41)])

-- 2F
-- True means "a p has occured"; false means it hasn't
fsa_asst2f :: Automaton Bool SegmentPKIU
fsa_asst2f = (False, [False, True], [(False, K, False),
                                     (False, I, False),
                                     (False, P, True),
                                     (True, P, True),
                                     (True, K, True),
                                     (True, I, True),
                                     (True, U, True)])

-- 2G
-- True means "a p has occured"; false means it hasn't
fsa_asst2g :: Automaton Bool SegmentPKIU
fsa_asst2g = (False, [False, True], [(False, K, False),
                                     (False, I, False),
                                     (False, P, True),
                                     (True, P, True),
                                     (True, K, False),
                                     (True, I, False),
                                     (True, U, False)])

-- 2H
-- state is form (there is at least one P, there is at least one U)
-- 41 represents (False, False)
-- 42 represents (True, False)
-- 43 represents (False, True)
-- 44 represents (True, True)
fsa_asst2h :: Automaton Int SegmentPKIU
fsa_asst2h = (41, [44], [(41, K, 41),
                         (41, I, 41),
                         (41, P, 42),
                         (41, U, 43),
                         (42, K, 42),
                         (42, I, 42),
                         (42, U, 44),
                         (43, K, 43),
                         (43, I, 43),
                         (43, P, 44),
                         (44, K, 44),
                         (44, I, 44),
                         (44, P, 44),
                         (44, U, 44)])