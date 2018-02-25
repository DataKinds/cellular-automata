module Main where

import World
import Automata
import Graphics.Gloss
import Debug.Trace

rule :: World Cell -> Cell
rule w = case w of
    (BaseDim [Off] Off [Off]) -> Off
    (BaseDim [Off] Off [On ]) -> On
    (BaseDim [Off] On  [Off]) -> Off
    (BaseDim [Off] On  [On])  -> Off
    (BaseDim [On]  Off [Off]) -> On
    (BaseDim [On]  Off [On ]) -> Off
    (BaseDim [On]  On  [Off]) -> Off
    (BaseDim [On]  On  [On])  -> Off

rectSize :: (Float, Float)
rectSize = (100, 100)
defaultRect :: Picture
defaultRect = (uncurry rectangleSolid) rectSize

shiftPictureList :: (Float, Float) -> [Picture] -> [Picture]
shiftPictureList (w, h) ps = zipWith (\(x, y) p -> translate x y p) offsetList ps
    where
        offsetList = zip [w, (w * 2)..] [h, (h * 2)..]

cellToPicture :: Cell -> Picture
cellToPicture On = color white defaultRect
cellToPicture Off = color (greyN 0.2) defaultRect

worldToPicture :: World Cell -> Picture
worldToPicture (BaseDim l c r) = pictures (concat [lPics, [cPic], rPics])
    where
        ls = take 20 l
        rs = take 20 r
        width = fst rectSize
        lPics = shiftPictureList (-width, 0) (map cellToPicture ls)
        cPic = cellToPicture c
        rPics = shiftPictureList (width, 0) (map cellToPicture rs)
worldToPicture (HigherDim u c d) = pictures (concat [uPics, [cPic], dPics])
    where
        us = take 20 u
        ds = take 20 d
        height = snd rectSize
        uPics = shiftPictureList (0, height) (map worldToPicture us)
        cPic = worldToPicture c
        dPics = shiftPictureList (0, -height) (map worldToPicture ds)

iterateWorld :: World Cell -> [Picture]
iterateWorld w = map (worldToPicture) (iterate (applyRule rule) w)
main :: IO ()
--main = mapM_ (putStrLn . show) (take 10 (iterate (applyRule rule) (write On genOneD)))
main = display (InWindow "Cellular Automata" (800, 800) (10, 10)) black pic
    where
        defaultWorld = write On genOneD
        pics = take 100 $ iterateWorld defaultWorld
        pic = pictures $ shiftPictureList (0, -(snd rectSize)) pics
