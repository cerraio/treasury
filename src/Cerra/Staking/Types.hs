{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Staking.Types
  ( StakingParams (..),
    StakingDatum (..),
    OracleDatum (..),
    StakingRedeemer (..)
  )
where

import Ledger.Value (AssetClass)
import Ledger (Address)
import qualified PlutusTx
import PlutusTx.Prelude (Eq, Integer, (&&), (==))
import qualified Prelude as Haskell

data StakingParams = StakingParams
  { spCerraAssetClass :: AssetClass,
    spOracleFTAssetClass :: AssetClass
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''StakingParams [('StakingParams, 0)]
PlutusTx.makeLift ''StakingParams

data StakingDatum = StakingDatum
  { sdOwnerAddress :: Address, -- owner of the staked CERRA
    sdEpoch        :: Integer, -- epoch, for which rewards were already paid (or initial epoch then staking action was performed)
    sdAdaPaid      :: Integer  -- how much ADA rewards paid for current epoch
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''StakingDatum [('StakingDatum, 0)]
PlutusTx.makeLift ''StakingDatum

instance Eq StakingDatum where
  {-# INLINEABLE (==) #-}
  x == y =
    sdOwnerAddress x == sdOwnerAddress y
      && sdEpoch x == sdEpoch y
      && sdAdaPaid x == sdAdaPaid y

data OracleDatum = OracleDatum
  { odTreasuryLovelace :: Integer, -- lovelace amount in the treasury at the time of the snapshot
    odStakingCerra     :: Integer, -- CERRA smallest indivisible unit amount in the staking at the time of the snapshot
    odEpoch            :: Integer -- epoch, for which the snapshot is provided
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''OracleDatum [('OracleDatum, 0)]
PlutusTx.makeLift ''OracleDatum

data StakingRedeemer = Unstake | Claim

PlutusTx.makeIsDataIndexed
  ''StakingRedeemer
  [ ('Unstake, 0),
    ('Claim, 1)
  ]
PlutusTx.makeLift ''StakingRedeemer
