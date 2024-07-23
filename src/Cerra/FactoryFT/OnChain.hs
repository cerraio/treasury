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

module Cerra.FactoryFT.OnChain
  ( mkFTScript,
    mkFTPolicy,
    mkFTSymbol,
  )
where

import Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)

import Cerra.FactoryFT.Types (FTParams (..))
import Cerra.Staking.Types
  ( StakingDatum (..)
  )
import Cerra.Utils.Settings (cerraAssetClass, stakingTokenName)
import Cerra.Staking.OnChain
  ( mkStakingScript
  )
import Cerra.Utils.Utils
  ( mustFindDatum,
    getUpperBound,
    getEpochFromPosixTime,
    validateSigner,
    getStakingContractInput,
    getStakingContractOutput
  )
import qualified Plutonomy
import Plutus.V2.Ledger.Contexts
  ( ownCurrencySymbol,
    ScriptContext,
    scriptContextTxInfo
  )
import Plutus.V2.Ledger.Api
  ( Script,
    TxInfo
      ( txInfoMint,
        txInfoValidRange,
        txInfoInputs,
        txInfoOutputs
      ),
    txOutValue,
    CurrencySymbol,
    MintingPolicy,
    unMintingPolicyScript,
    txOutDatum,
    ValidatorHash,
    Validator (..)
  )
import Plutus.Script.Utils.V2.Scripts (validatorHash)
import Ledger.Value
  ( AssetClass,
    Value,
    assetClass,
    assetClassValueOf
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( BuiltinData,
    ($),
    (&&),
    (==),
    (>),
    Bool (..)
  )

--import Cerra.Utils.Debug (debugError)

stakingValidatorHash :: ValidatorHash
stakingValidatorHash = validatorHash $ Validator $ mkStakingScript

{-# INLINEABLE mkFTPolicy #-}
mkFTPolicy :: MintingPolicy
mkFTPolicy =
  Plutonomy.optimizeUPLC $
    Plutonomy.mintingPolicyToPlutus originalFTPolicy

{-# INLINEABLE mkFTScript #-}
mkFTScript :: Script
mkFTScript = unMintingPolicyScript mkFTPolicy

{-# INLINABLE originalFTPolicy #-}
originalFTPolicy :: Plutonomy.MintingPolicy
originalFTPolicy = Plutonomy.mkMintingPolicyScript ($$(PlutusTx.compile [|| \param' -> mkUntypedMintingPolicy $ mkFTValidator param' ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
  where
    params =
      FTParams
        { fpStakingValidatorHash = stakingValidatorHash,
          fpCerraAssetClass = cerraAssetClass,
          fpStakingTokenName = stakingTokenName
        }

{-# INLINEABLE mkFTSymbol #-}
mkFTSymbol :: CurrencySymbol
mkFTSymbol = scriptCurrencySymbol mkFTPolicy

-- 1. validate that only one asset was burned, and execute additional checks
-- 2. validate that only one asset was minted, and execute additional checks
-- 3. fail otherwise
{-# INLINEABLE mkFTValidator #-}
mkFTValidator :: FTParams -> BuiltinData -> ScriptContext -> Bool
mkFTValidator FTParams {fpStakingValidatorHash, fpCerraAssetClass, fpStakingTokenName} _ context =
  let info = scriptContextTxInfo context

      minted :: Value
      minted = txInfoMint info

      fTAssetClass = assetClass (ownCurrencySymbol context) fpStakingTokenName

     in if assetClassValueOf minted fTAssetClass == -1 then validateBurn info fpStakingValidatorHash
        else if assetClassValueOf minted fTAssetClass == 1 then validateMint info fTAssetClass fpCerraAssetClass fpStakingValidatorHash
        else False

-- 1. ensure that there is only 1 output at staking script address
-- 2. ensure that minted unit is in that output
-- 3. ensure that output at staking contract contains CERRA tokens (we do not care about ADA or other tokens)
-- 4. ensure that ADA paid in Datum is set to 0
-- 5. ensure that current epoch is supplied through PosixTime calculations
-- 6. ensure that signer address is in the datum
-- FT minting already validated in main function
{-# INLINEABLE validateMint #-}
validateMint :: TxInfo -> AssetClass -> AssetClass -> ValidatorHash -> Bool
validateMint info fTAssetClass fpCerraAssetClass fpStakingValidatorHash =
  let txOutputs = txInfoOutputs info

      stakingOutput = getStakingContractOutput txOutputs fpStakingValidatorHash -- 1.

      stakingAssets = txOutValue stakingOutput

      stakingDatum = mustFindDatum @StakingDatum (txOutDatum stakingOutput) info

      upperBound = getUpperBound (txInfoValidRange info)

      expectedStakingDatum =
        StakingDatum
          { sdOwnerAddress = sdOwnerAddress stakingDatum,
            sdEpoch = getEpochFromPosixTime upperBound, -- 5.
            sdAdaPaid = 0 -- 4.
          }

     in assetClassValueOf stakingAssets fTAssetClass == 1 -- 2.
         && assetClassValueOf stakingAssets fpCerraAssetClass > 0 -- 3.
         && stakingDatum == expectedStakingDatum -- 4., 5., 6.
         && validateSigner stakingDatum info -- 6.

-- as enforced in minting, FT can only appear in staking contract, thus there is no need to validate input Value or origin
-- we do not care where assets are going, as long as the transaction is signed by the owner and FT is burned
-- 1. ensure that there is only one input with datum
-- 2. ensure that this input is from Staking Contract
-- 3. ensure that signer address is in the datum
-- FT burning already validated in main function
{-# INLINEABLE validateBurn #-}
validateBurn :: TxInfo -> ValidatorHash -> Bool
validateBurn info fpStakingValidatorHash =
  let txInputs = txInfoInputs info

      stakingInput = getStakingContractInput txInputs fpStakingValidatorHash -- 1., 2.

      stakingDatum = mustFindDatum @StakingDatum (txOutDatum stakingInput) info

     in validateSigner stakingDatum info -- 3.
