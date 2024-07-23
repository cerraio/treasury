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

module Cerra.Staking.OnChain
  ( mkStakingScript
  )
where

import Plutus.V2.Ledger.Api
   ( TxInfo
      ( txInfoInputs,
        txInfoMint
      ),
    TxInInfo
      (
      txInInfoResolved
      ),
    TxOut,
    Script,
    ScriptContext (scriptContextTxInfo),
    unValidatorScript,
    txOutValue,
    txInfoFee,
    txInfoReferenceInputs
  )
import Plutus.V2.Ledger.Contexts
  ( ownHash,
    getContinuingOutputs,
    txOutDatum,
    valueSpent,
    valuePaidTo
  )
import Ledger
  ( Value,
    toPubKeyHash
  )
import Cerra.Staking.Types
  ( StakingParams (..),
    StakingDatum (..),
    OracleDatum (..),
    StakingRedeemer (..)
  )
import Cerra.Utils.Settings (cerraAssetClass, oracleFTAssetClass, minimumLovelaceStep)
import Cerra.Utils.Utils
  ( mustFindDatum,
    getStakingContractInput,
    getCS,
    getTN,
    getAmount,
    adaCoin
  )
import qualified Plutonomy
import Ledger.Value
  ( AssetClass,
    assetClassValueOf,
    flattenValue,
    valueOf
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinData,
    Maybe (Just, Nothing),
    error,
    divide,
    ($),
    (&&),
    (/=),
    (*),
    (+),
    (-),
    (/=),
    (<=),
    (==),
    (>),
  )

mkStakingScript :: Script
mkStakingScript =
  unValidatorScript $
    Plutonomy.optimizeUPLC $
      Plutonomy.validatorToPlutus originalStakingScript

originalStakingScript :: Plutonomy.Validator
originalStakingScript =
  Plutonomy.mkValidatorScript
    ( $$(PlutusTx.compile [||mkStakingValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode stakingParams
    )
  where
    !stakingParams =
        StakingParams
          { spCerraAssetClass = cerraAssetClass,
            spOracleFTAssetClass = oracleFTAssetClass
          }

{-# INLINEABLE mkStakingValidator #-}
mkStakingValidator ::
  StakingParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
mkStakingValidator StakingParams {spCerraAssetClass, spOracleFTAssetClass} rawDatum rawRedeemer rawContext =
  if case redeemer of
    Unstake ->
      validateUnstake ctx
    Claim ->
      validateClaim spCerraAssetClass spOracleFTAssetClass datum ctx
    then ()
    else error()
  where
    redeemer = PlutusTx.unsafeFromBuiltinData @StakingRedeemer rawRedeemer
    datum = PlutusTx.unsafeFromBuiltinData @StakingDatum rawDatum
    ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext rawContext

validateClaim ::
  AssetClass ->
  AssetClass ->
  StakingDatum ->
  ScriptContext ->
  Bool
validateClaim
  spCerraAssetClass
  spOracleFTAssetClass
  inStakingDatum
  ctx =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        txInputs = txInfoInputs info

        stakingInput = getStakingContractInput txInputs (ownHash ctx)
        stakingInputValue = txOutValue stakingInput
        stakingInputAda = assetClassValueOf stakingInputValue adaCoin
        stakingInputCerra = assetClassValueOf stakingInputValue spCerraAssetClass

        transactionFeeAda = assetClassValueOf (txInfoFee info) adaCoin

        stakingOutput :: TxOut
        stakingOutput = case getContinuingOutputs ctx of
          [o] -> o
          _ -> error()

        stakingOutputValue = txOutValue stakingOutput

        outStakingDatum = mustFindDatum @StakingDatum (txOutDatum stakingOutput) info

        totalInputValue = valueSpent info

        totalInputAda = assetClassValueOf totalInputValue adaCoin

        adaSentToUser = totalInputAda - stakingInputAda -- actually paid reward amount including fee

        oracleInput = case txInfoReferenceInputs info of
          [o] -> txInInfoResolved o
          _ -> error()

        oracleValue = txOutValue oracleInput
        oracleDatum = mustFindDatum @OracleDatum (txOutDatum oracleInput) info
        oracleEpoch = odEpoch oracleDatum

        ratio = ((odTreasuryLovelace oracleDatum) * 1000000) `divide` (odStakingCerra oracleDatum)
        totalAdaReward = (stakingInputCerra * ratio) `divide` 1000000 -- whole user reward amount

        actuallyPaid = adaSentToUser + sdAdaPaid inStakingDatum -- total ADA amount paid to user

        isFullyPaidIncludingStep = actuallyPaid + minimumLovelaceStep > totalAdaReward -- is reward fully paid, considering minimum ADA payment step.

        ownerAddress = sdOwnerAddress inStakingDatum
        payoutEpoch = sdEpoch inStakingDatum

        expectedStakingDatum =
          StakingDatum
            { sdOwnerAddress = ownerAddress,
              sdEpoch = if isFullyPaidIncludingStep then oracleEpoch - 1 else payoutEpoch, -- if everything is paid, we set following epoch. if not, we will need to do payout one more time
              sdAdaPaid = if isFullyPaidIncludingStep then 0 else actuallyPaid -- if everything is paid, we reset paid amount, if not, we keep it for next iteration
            }

        recipient = case toPubKeyHash ownerAddress of
          Just pkh -> pkh
          Nothing -> error()

        adaReceivedByUser = (assetClassValueOf (valuePaidTo info recipient) adaCoin) + transactionFeeAda -- we count the fee towards datum adaPaid field, but then doing actually paid check, we need to minus out the fee, as it was paid from treasury UTXO

     -- by the guards enforced below, minimum reward payout will be 2 ADA, as there won't be UTXOs at treasury with less than 2 ADA
     in actuallyPaid <= totalAdaReward -- ensure we are not paying more than total maximum reward
        && expectedStakingDatum == outStakingDatum -- mutated inStakingDatum, must match outStakingDatum, which defines if rewards are fully paid
        && (payoutEpoch + 2) <= oracleEpoch -- reward payments are trailing by one epoch
        && stakingInputValue == stakingOutputValue -- staking contract input and output must stay the same - containing same deposit ADA, CERRA and FactoryFT
        && adaSentToUser == adaReceivedByUser -- all of the input ADA (except ADA from staking UTXO and transaction fee) must go to the address set in Staking Datum
        && assetClassValueOf oracleValue spOracleFTAssetClass == 1 -- validate treasury oracle reference input authenticity
        && transactionFeeAda <= 1_500_000 -- anyone can submit the tx and choose tx fees. this ensures at least some limit

-- we need to ensure, that FactoryFT is burned. Burning it triggers the main validation logic
-- FactoryFT minting policy ensures, that FT is in this script, and its amount is 1
-- if we enforce burning of all of the tokens which have amount 1, when we are sure, that FactoryFT minting policy will be triggered, if token is in the UTXO
-- if someone sends funds without minting the FactoryFT, anyone will be able to consume this UTXO, that is not our problem
-- logic is simplified in this validator (transferred to minting policy) in order to make Claim execution units smaller
validateUnstake :: ScriptContext -> Bool
validateUnstake ctx =
    let info :: TxInfo
        info = scriptContextTxInfo ctx

        minted :: Value
        minted = txInfoMint info

        txInputs = txInfoInputs info

        stakingInput = getStakingContractInput txInputs (ownHash ctx)

        tokens = flattenValue (txOutValue stakingInput)

     in case [t | t <- tokens, getAmount t == 1 && valueOf minted (getCS t) (getTN t) /= -1] of
          [] -> True -- if we can't find any tokens with amount 1 which were not burned
          _ -> False -- in any other case
