module Main where

import Build_doctests (flags, pkgs, module_sources)
import Test.DocTest (doctest)

main :: IO ()
main = doctest
  $ "-fobject-code"
  --- ^ Use object code to work around https://gitlab.haskell.org/ghc/ghc/-/issues/19460
  -- in GHC 9.0.1.
  : flags ++ pkgs ++ module_sources
