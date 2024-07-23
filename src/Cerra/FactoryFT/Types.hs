{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.FactoryFT.Types
  ( FTParams (..),
  )
where

import Plutus.V2.Ledger.Api (ValidatorHash)
import Ledger.Value(AssetClass, TokenName)
import qualified PlutusTx
import qualified Prelude as Haskell

data FTParams = FTParams
  { fpStakingValidatorHash :: ValidatorHash,
    fpCerraAssetClass :: AssetClass,
    fpStakingTokenName :: TokenName
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''FTParams [('FTParams, 0)]
PlutusTx.makeLift ''FTParams
