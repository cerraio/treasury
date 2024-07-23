{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Treasury.Types
  ( TreasuryParams (..),
  )
where

import Ledger.Value (CurrencySymbol, TokenName)
import qualified PlutusTx
import qualified Prelude as Haskell

data TreasuryParams = TreasuryParams
  { tpFactoryFTSymbol :: CurrencySymbol,
    tpStakingTokenName :: TokenName
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''TreasuryParams [('TreasuryParams, 0)]
PlutusTx.makeLift ''TreasuryParams
