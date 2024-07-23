{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Cerra.Treasury.OnChain
  ( mkTreasuryScript,
  )
where

import Plutus.V2.Ledger.Api
  ( ScriptContext (scriptContextTxInfo),
    Script,
    unValidatorScript
  )
import Plutus.V2.Ledger.Contexts
  ( valueSpent
  )
import Ledger.Value (assetClassValueOf, assetClass)
import Cerra.Treasury.Types
  ( TreasuryParams (..)
  )

import qualified Plutonomy
import qualified PlutusTx
import PlutusTx.Prelude
  ( BuiltinData,
    ($),
    (==),
    error
  )

import Cerra.FactoryFT.OnChain (mkFTSymbol)
import Cerra.Utils.Settings (stakingTokenName)
--import Cerra.Utils.Debug (debugError)

mkTreasuryScript :: Script
mkTreasuryScript =
  unValidatorScript $
    Plutonomy.optimizeUPLC $
      Plutonomy.validatorToPlutus originalTreasuryScript

originalTreasuryScript :: Plutonomy.Validator
originalTreasuryScript =
  Plutonomy.mkValidatorScript
    ( $$(PlutusTx.compile [||mkTreasuryValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode treasuryParams
    )
  where
    !treasuryParams =
        TreasuryParams
          { tpFactoryFTSymbol = mkFTSymbol,
            tpStakingTokenName = stakingTokenName
          }

{-# INLINEABLE mkTreasuryValidator #-}
mkTreasuryValidator ::
  TreasuryParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkTreasuryValidator
  TreasuryParams
    { tpFactoryFTSymbol,
      tpStakingTokenName
    }
  _
  _
  rawContext =
    if assetClassValueOf assets (assetClass tpFactoryFTSymbol tpStakingTokenName) == 1
        then ()
        else error()
      where
        ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext rawContext -- we have builtinData here, so maybe we can find only tx inputs, and only then serialize, this way reducing transaction costs significantly
        info = scriptContextTxInfo ctx
        assets = valueSpent info
