{-# LANGUAGE DeriveFunctor #-}

module World where

import Data.List

data Cell = On | Off
instance Show Cell where
    show On = "◆"
    show Off = "◇"

data World a = BaseDim [Cell] Cell [Cell] | HigherDim [World a] (World a) [World a]
    deriving (Functor)

instance Show (World a) where
    show (BaseDim l c r) = (listString $ reverse shortLs) ++ "  " ++ (show c) ++ "  " ++ (listString shortRs)
        where
            shortLs = take 4 l
            shortRs = take 4 r
            listString ls = intercalate " " $ map show ls
    show (HigherDim u c d) = (listString $ reverse shortUs) ++ "\n\n" ++ (show c) ++ "\n\n" ++ (listString shortDs)
        where
            shortUs = take 4 u
            shortDs = take 4 d
            listString ls = intercalate "\n" $ map show ls

left :: World a -> World a
left (BaseDim (l:ls) c rs) = BaseDim ls l (c:rs)
left (HigherDim us c ds) = HigherDim (map left us) (left c) (map left ds)
right :: World a -> World a
right (BaseDim ls c (r:rs)) = BaseDim (c:ls) r rs
right (HigherDim us c ds) = HigherDim (map right us) (right c) (map right ds)
up :: World a -> World a
up w@(BaseDim l c r) = w
up (HigherDim (u:us) c ds) = HigherDim us u (c:ds)
down :: World a -> World a
down w@(BaseDim l c r) = w
down (HigherDim us c (d:ds)) = HigherDim (c:us) d ds

getNeighborhood :: World a -> World a
getNeighborhood (BaseDim ls c rs) = BaseDim (take radius ls) c (take radius rs)
    where
        radius = 1
getNeighborhood (HigherDim us c ds) = HigherDim (map getNeighborhood (take radius us)) (getNeighborhood c) (map getNeighborhood (take radius ds))
    where
        radius = 1

write :: Cell -> World a -> World a
write c' (BaseDim l c r) = BaseDim l c' r
write c' (HigherDim u c d) = HigherDim u (write c' c) d

defaultCell :: Cell
defaultCell = Off
genOneD :: World a
genOneD = BaseDim (repeat defaultCell) defaultCell (repeat defaultCell)
genTwoD :: World a
genTwoD = HigherDim (repeat genOneD) genOneD (repeat genOneD)
