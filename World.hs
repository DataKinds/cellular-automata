{-# LANGUAGE DeriveFunctor #-}

module World where

import Data.List

data Cell = On | Off
instance Show Cell where
    show On = "◆"
    show Off = "◇"

class World a where
    up :: a -> a
    down :: a -> a
    left :: a -> a
    right :: a -> a
    --write :: b -> (a b) -> (a b)
    getNeighborhood :: a -> a

data OneD a = OneD [a] a [a] deriving (Functor)
instance World (OneD a) where
    left (OneD (l:ls) c rs) = OneD ls l (c:rs)
    right (OneD ls c (r:rs)) = OneD (c:ls) r rs
    up = id
    down = id
    --write c' (OneD ls c rs) = OneD ls c' rs
    getNeighborhood (OneD ls c rs) = OneD (take radius ls) c (take radius rs)
        where
            radius = 1
instance (Show a) => Show (OneD a) where
    show (OneD ls c rs) = (listString $ reverse shortLs) ++ "  " ++ (show c) ++ "  " ++ (listString shortRs)
        where
            shortLs = take 4 ls
            shortRs = take 4 rs
            listString l = intercalate " " $ map show l


data TwoD a = TwoD [OneD a] (OneD a) [OneD a] deriving (Functor)
instance World (TwoD a) where
    up (TwoD (u:us) c ds) = TwoD us u (c:ds)
    down (TwoD us c (d:ds)) = TwoD (c:us) d ds
    left (TwoD us c ds) = TwoD (map left us) (left c) (map left ds)
    right (TwoD us c ds) = TwoD (map right us) (right c) (map right ds)
    --write c' (TwoD us c ds) = TwoD us (write c' c) ds
    getNeighborhood (TwoD us c ds) = TwoD (map getNeighborhood (take radius us)) (getNeighborhood c) (map getNeighborhood (take radius ds))
        where
            radius = 1
instance (Show a) => Show (TwoD a) where
    show (TwoD us c ds) = (listString $ reverse shortUs) ++ "\n\n" ++ (show c) ++ "\n\n" ++ (listString shortDs)
        where
            shortUs = take 4 us
            shortDs = take 4 ds
            listString l = intercalate "\n" $ map show l


defaultCell :: Cell
defaultCell = Off
genOneD :: OneD Cell
genOneD = OneD (repeat defaultCell) defaultCell (repeat defaultCell)
genTwoD :: TwoD Cell
genTwoD = TwoD (repeat genOneD) genOneD (repeat genOneD)
