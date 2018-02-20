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
    getNeighborhood :: a -> a
data Dim a = Dim [a] a [a] deriving (Functor)
newtype OneD = OneD (Dim Cell)
newtype TwoD = TwoD (Dim OneD)

instance World (OneD) where
    left (OneD (Dim (l:ls) c rs)) = OneD (Dim ls l (c:rs))
    right (OneD (Dim ls c (r:rs))) = OneD (Dim (c:ls) r rs)
    up = id
    down = id
    getNeighborhood (OneD (Dim ls c rs)) = OneD (Dim (take radius ls) c (take radius rs))
        where
            radius = 1
instance Show (OneD) where
    show (OneD (Dim ls c rs)) = (listString $ reverse shortLs) ++ "  " ++ (show c) ++ "  " ++ (listString shortRs)
        where
            shortLs = take 4 ls
            shortRs = take 4 rs
            listString l = intercalate " " $ map show l


--data TwoD a = TwoD [OneD a] (OneD a) [OneD a] deriving (Functor)
instance World (TwoD) where
    up (TwoD (Dim (u:us) c ds)) = TwoD (Dim us u (c:ds))
    down (TwoD (Dim us c (d:ds))) = TwoD (Dim (c:us) d ds)
    left (TwoD (Dim us c ds)) = TwoD (Dim (map left us) (left c) (map left ds))
    right (TwoD (Dim us c ds)) = TwoD (Dim (map right us) (right c) (map right ds))
    getNeighborhood (TwoD (Dim us c ds)) = TwoD (Dim (map getNeighborhood (take radius us)) (getNeighborhood c) (map getNeighborhood (take radius ds)))
        where
            radius = 1
instance Show (TwoD) where
    show (TwoD (Dim us c ds)) = (listString $ reverse shortUs) ++ "\n\n" ++ (show c) ++ "\n\n" ++ (listString shortDs)
        where
            shortUs = take 4 us
            shortDs = take 4 ds
            listString l = intercalate "\n" $ map show l

--write :: (World a) => b -> a b -> a b
--write c' w =
--write c' (TwoD us c ds) = TwoD us (write c' c) ds

defaultCell :: Cell
defaultCell = Off
genOneD :: OneD
genOneD = OneD (Dim (repeat defaultCell) defaultCell (repeat defaultCell))
genTwoD :: TwoD
genTwoD = TwoD (Dim (repeat genOneD) genOneD (repeat genOneD))
