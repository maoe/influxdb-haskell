module Main where
import Test.DocTest (doctest)

import Build_doctests (flags, pkgs, module_sources)

main :: IO ()
main = do
  -- traverse_ putStrLn args
  doctest args
  where
    args = flags ++ pkgs ++ module_sources
