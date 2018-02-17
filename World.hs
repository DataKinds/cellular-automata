module World where

import Data.List

data Cell = On | Off
instance Show Cell where
    show On = "◆"
    show Off = "◇"

class World dim where
    up :: dim -> dim
    down :: dim -> dim
    left :: dim -> dim
    right :: dim -> dim
    write :: Cell -> dim -> dim

data OneD = OneD [Cell] Cell [Cell]
instance World OneD where
    left (OneD (l:ls) c rs) = OneD ls l (c:rs)
    right (OneD ls c (r:rs)) = OneD (c:ls) r rs
    up = id
    down = id
    write c' (OneD ls c rs) = OneD ls c' rs
instance Show OneD where
    show (OneD ls c rs) = (listString $ reverse shortLs) ++ "  " ++ (show c) ++ "  " ++ (listString shortRs)
        where
            shortLs = take 4 ls
            shortRs = take 4 rs
            listString l = intercalate " " $ map show l


data TwoD = TwoD [OneD] OneD [OneD]
instance World TwoD where
    up (TwoD (u:us) c ds) = TwoD us u (c:ds)
    down (TwoD us c (d:ds)) = TwoD (c:us) d ds
    left (TwoD us c ds) = TwoD (map left us) (left c) (map left ds)
    right (TwoD us c ds) = TwoD (map right us) (right c) (map right ds)
    write c' (TwoD us c ds) = TwoD us (write c' c) ds
instance Show TwoD where
    show (TwoD us c ds) = (listString $ reverse shortUs) ++ "\n\n" ++ (show c) ++ "\n\n" ++ (listString shortDs)
        where
            shortUs = take 4 us
            shortDs = take 4 ds
            listString l = intercalate "\n" $ map show l

defaultCell :: Cell
defaultCell = Off
genOneD :: OneD
genOneD = OneD (repeat defaultCell) defaultCell (repeat defaultCell)
genTwoD :: TwoD
genTwoD = TwoD (repeat genOneD) genOneD (repeat genOneD)
