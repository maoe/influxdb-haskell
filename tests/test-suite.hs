{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Applicative
import Control.Exception as E
import Control.Monad.Trans
import Data.Int
import Data.List (find)
import Data.Monoid
import Data.Text (Text)
import Data.Unique
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import Test.Tasty.HUnit
import Test.Tasty.TH

import Database.InfluxDB

main :: IO ()
main = $defaultMainGenerator
