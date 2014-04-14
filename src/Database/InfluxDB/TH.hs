{-# LANGUAGE TemplateHaskell #-}
module Database.InfluxDB.TH
  ( Options(..), defaultOptions
  -- , deriveSeriesData
  -- , deriveToSeriesData
  , deriveFromSeriesData

  , stripPrefixLower
  ) where
import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)

import Database.InfluxDB.Decode
import Database.InfluxDB.Types.Internal (stripPrefixLower)

data Options = Options
  { fieldLabelModifier :: String -> String
  }

defaultOptions :: Options
defaultOptions = Options
  { fieldLabelModifier = id
  }

-- deriveSeriesData :: Options -> Name -> Q [Dec]
-- deriveSeriesData opts name = (++)
--   <$> deriveToSeriesData opts name
--   <*> deriveFromSeriesData opts name

-- deriveToSeriesData :: Options -> Name -> Q [Dec]
-- deriveToSeriesData _opts _name = do
--   return undefined

deriveFromSeriesData :: Options -> Name -> Q [Dec]
deriveFromSeriesData opts name = do
  info <- reify name
  case info of
    TyConI dec -> pure <$> decToFromSeriesData opts dec
    _ -> fail $ "Expected a type constructor, but got " ++ show info

decToFromSeriesData :: Options -> Dec -> Q Dec
decToFromSeriesData opts dec = case dec of
  DataD _ tyName tyVars [con] _ -> fromSeriesDataBody opts tyName tyVars con
  NewtypeD _ tyName tyVars con _ -> fromSeriesDataBody opts tyName tyVars con
  _ -> fail $ "Expected a data or newtype declaration, but got " ++ show dec

fromSeriesDataBody :: Options -> Name -> [TyVarBndr] -> Con -> Q Dec
fromSeriesDataBody opts tyName tyVars con = do
  case con of
    RecC conName vars -> instanceD
      (mapM tyVarToPred tyVars)
      [t| FromSeriesData $(conT tyName) |]
      [deriveParseSeriesData conName vars]
    _ -> fail $ "Expected a record, but got " ++ show con
  where
    tyVarToPred tv = case tv of
      PlainTV name -> classP ''FromValue [varT name]
      _ -> fail $ "Expected PlainTV, but got " ++ show tv
    deriveParseSeriesData conName vars = funD 'parseSeriesData
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
