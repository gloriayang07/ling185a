{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Prelude hiding (init)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring a) => a -> a -> a -> a
distrib_lhs x y z = gconj x (gdisj y z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

