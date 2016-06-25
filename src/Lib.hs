-- This does nothing; it's here as a template should we want a local
-- library later; see the Lib section in hblog.cabal in that case
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
