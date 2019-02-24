module Memoization (memoize, lift0, lift1, lift2, liftList) where

import Control.Monad
import qualified Data.Map as Map

fix :: (a -> a) -> a
fix f = let x = f x in x

data TableBased c v a = MkTableBased (Map.Map c v -> (a, Map.Map c v))

instance Functor (TableBased c v) where
    fmap = liftM
instance Applicative (TableBased c v) where
    pure x = MkTableBased (\n -> (x,n))
    (<*>) = ap
instance Monad (TableBased c v) where
    -- DIY state monad
    (MkTableBased fa) >>= k =
        MkTableBased $ \n ->
            let (a,n') = fa n in
            let (MkTableBased fb) = k a in
            let (b,n'') = fb n' in
            (b,n'')

liftList :: ([a] -> a) -> [TableBased c a a] -> TableBased c a a
liftList f xs = liftM f (sequence xs)

lift2 :: (x -> y -> z) -> TableBased c a x -> TableBased c a y -> TableBased c a z
lift2 = liftM2

lift1 :: (x -> y) -> TableBased c a x -> TableBased c a y
lift1 = liftM

lift0 :: x -> TableBased c a x
lift0 = pure

tryRetrieveElse :: (Ord c) => (c -> TableBased c a a) -> c -> TableBased c a a
tryRetrieveElse f c =
    let yield x = MkTableBased (\tbl -> (x, Map.insert c x tbl)) in
    MkTableBased (\tbl -> (Map.lookup c tbl, tbl)) >>= (\r ->
        case r of
        Just x -> pure x
        Nothing -> (f c) >>= yield
    )

goFromEmptyTable :: TableBased c a a -> a
goFromEmptyTable (MkTableBased f) = fst (f Map.empty)

memoize :: (Ord c) => ((c -> TableBased c a a) -> (c -> TableBased c a a)) -> c -> a
memoize f = \x -> goFromEmptyTable (fix (tryRetrieveElse . f) x)

---------------------------------------------
-- A demo example: a fibonacci function
-- and a faster memoized version.

fib :: Int -> Int
fib n = if n <= 1 then 1 else (fib (n-1) + fib (n-2))

fastFib :: Int -> Int
fastFib =
    memoize (\self -> \n ->
                if n <= 1 then
                    pure 1
                else
                    liftM2 (+) (self (n-1)) (self (n-2))
    )

