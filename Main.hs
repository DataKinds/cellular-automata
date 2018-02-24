module Main where

import World
import Automata

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


main :: IO ()
main = mapM_ (putStrLn . show) (take 10 (iterate (applyRule rule) (write On genOneD)))
