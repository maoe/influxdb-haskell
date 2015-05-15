{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ == 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Database.InfluxDB.TH
  ( Options(..), defaultOptions
  , deriveSeriesData
  , deriveToSeriesData
  , deriveFromSeriesData

  , stripPrefixLower
  , stripPrefixSnake
  ) where
import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)
import Prelude

import qualified Data.Vector as V

import Database.InfluxDB.Decode
import Database.InfluxDB.Encode
import Database.InfluxDB.Types.Internal

data Options = Options
  { fieldLabelModifier :: String -> String
  }

defaultOptions :: Options
defaultOptions = Options
  { fieldLabelModifier = id
  }

deriveSeriesData :: Options -> Name -> Q [Dec]
deriveSeriesData opts name = (++)
  <$> deriveToSeriesData opts name
  <*> deriveFromSeriesData opts name

deriveToSeriesData :: Options -> Name -> Q [Dec]
deriveToSeriesData opts name = do
  info <- reify name
  case info of
    TyConI dec -> pure <$> deriveWith toSeriesDataBody opts dec
    _ -> fail $ "Expected a type constructor, but got " ++ show info

deriveFromSeriesData :: Options -> Name -> Q [Dec]
deriveFromSeriesData opts name = do
  info <- reify name
  case info of
    TyConI dec -> pure <$> deriveWith fromSeriesDataBody opts dec
    _ -> fail $ "Expected a type constructor, but got " ++ show info

deriveWith
  :: (Options -> Name -> [TyVarBndr] -> Con -> Q Dec)
  -> Options -> Dec -> Q Dec
deriveWith f opts dec = case dec of
  DataD _ tyName tyVars [con] _ -> f opts tyName tyVars con
  NewtypeD _ tyName tyVars con _ -> f opts tyName tyVars con
  _ -> fail $ "Expected a data or newtype declaration, but got " ++ show dec

toSeriesDataBody :: Options -> Name -> [TyVarBndr] -> Con -> Q Dec
toSeriesDataBody opts tyName tyVars con = do
  case con of
    RecC conName vars -> InstanceD
      <$> mapM tyVarToPred tyVars
      <*> [t| ToSeriesData $(conT tyName) |]
      <*> deriveDecs conName vars
    _ -> fail $ "Expected a record, but got " ++ show con
  where
    tyVarToPred tv = case tv of
#if MIN_VERSION_template_haskell(2, 10, 0)
      PlainTV name -> conT ''FromValue `appT` varT name
      KindedTV name _ -> conT ''FromValue `appT` varT name
#else
      PlainTV name -> classP ''FromValue [varT name]
      KindedTV name _ -> classP ''FromValue [varT name]
#endif
    deriveDecs _conName vars = do
      a <- newName "a"
      sequence
        [ funD 'toSeriesColumns
          [ clause [wildP]
            (normalB [| V.fromList $(listE columns) |]) []
          ]
        , funD 'toSeriesPoints
          [ clause [varP a]
            (normalB [| V.fromList $(listE $ map (applyToValue a) vars) |]) []
          ]
        ]
      where
        applyToValue a (name, _, _) = [| toValue ($(varE name) $(varE a)) |]
        columns = map (varStrictTypeToColumn opts) vars

fromSeriesDataBody :: Options -> Name -> [TyVarBndr] -> Con -> Q Dec
fromSeriesDataBody opts tyName tyVars con = do
  case con of
    RecC conName vars -> instanceD
      (mapM tyVarToPred tyVars)
      [t| FromSeriesData $(conT tyName) |]
      [deriveDec conName vars]
    _ -> fail $ "Expected a record, but got " ++ show con
  where
    tyVarToPred tv = case tv of
#if MIN_VERSION_template_haskell(2, 10, 0)
      PlainTV name -> conT ''FromValue `appT` varT name
      KindedTV name _ -> conT ''FromValue `appT` varT name
#else
      PlainTV name -> classP ''FromValue [varT name]
      KindedTV name _ -> classP ''FromValue [varT name]
#endif
    deriveDec conName vars = funD 'parseSeriesData
      [ clause [] (normalB deriveBody) []
      ]
      where
        deriveBody = do
          values <- newName "values"
          appE (varE 'withValues) $ lamE [varP values] $
              foldl (go values) [| pure $(conE conName) |] columns
          where
            go :: Name -> Q Exp -> Q Exp -> Q Exp
            go values expQ col = [| $expQ <*> $(varE values) .: $col |]
            columns = map (varStrictTypeToColumn opts) vars

varStrictTypeToColumn :: Options -> VarStrictType -> Q Exp
varStrictTypeToColumn opts = column opts . f
  where
    f (var, _, _) = var

column :: Options -> Name -> Q Exp
column opts = litE . stringL . fieldLabelModifier opts . nameBase
