{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cerra.Utils.Utils
  ( mustFindDatum,
    getUpperBound,
    getEpochFromPosixTime,
    validateSigner,
    getStakingContractInput,
    getStakingContractOutput,
    scriptDatumExists,
    getCS,
    getTN,
    getAmount,
    adaCoin
  )
where

import Data.Maybe (fromJust)

import Plutus.V2.Ledger.Api
    ( TxInInfo,
      txOutAddress,
      txInInfoResolved,
      ValidatorHash,
      Datum (Datum),
      txOutDatum,
      OutputDatum(..),
      getDatum,
      POSIXTime
    )
import Plutus.V2.Ledger.Contexts (txSignedBy, TxInfo, TxOut, findDatum)
import Ledger (toPubKeyHash, toValidatorHash, getPOSIXTime)
import Ledger.Value (AssetClass, assetClass, adaSymbol, adaToken)
import Ledger.Interval (Interval, UpperBound (..), Extended(Finite), ivTo)
import Cerra.Staking.Types(StakingDatum (..))
import qualified PlutusTx
import PlutusTx.IsData.Class (UnsafeFromData)
import PlutusTx.Prelude

--import Cerra.Utils.Debug (debugError)

{-# INLINEABLE mustFindDatum #-}
mustFindDatum :: (UnsafeFromData d) => OutputDatum -> TxInfo -> d
mustFindDatum od info = case od of
  OutputDatum dat -> PlutusTx.unsafeFromBuiltinData (getDatum dat)
  OutputDatumHash dh -> case findDatum dh info of
        Just (Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
        _ -> error()
  NoOutputDatum -> error()

{-# INLINEABLE getUpperBound #-}
getUpperBound :: Interval a -> a
getUpperBound interval = case ivTo interval of
    UpperBound (Finite value) _isInclusive -> fromJust (Just value)
    _ -> error()

{-# INLINEABLE getEpochFromPosixTime #-}
-- mainnet
--getEpochFromPosixTime :: POSIXTime -> Integer
--getEpochFromPosixTime time = ((getPOSIXTime time) - 1506203091000) `divide` 432000000
-- testnet
getEpochFromPosixTime :: POSIXTime -> Integer
getEpochFromPosixTime time = ((getPOSIXTime time) - 1654041618000) `divide` 432000000

{-# INLINEABLE validateSigner #-}
validateSigner :: StakingDatum -> TxInfo -> Bool
validateSigner stakingDatum info =
  let sender = sdOwnerAddress stakingDatum

      senderPubKeyHash = case toPubKeyHash sender of
        Just pkh -> pkh
        Nothing -> error()

     in txSignedBy info senderPubKeyHash

{-# INLINABLE getStakingContractInput #-}
getStakingContractInput :: [TxInInfo] -> ValidatorHash -> TxOut
getStakingContractInput inputs vh =
  case [i | i <- inputs, (toValidatorHash $ txOutAddress $ txInInfoResolved i) == Just vh] of
    [i] -> txInInfoResolved i
    _ -> error()

{-# INLINABLE getStakingContractOutput #-}
getStakingContractOutput :: [TxOut] -> ValidatorHash -> TxOut
getStakingContractOutput outputs vh = if (fromJustCustom $ toValidatorHash $ txOutAddress output) == vh
  then output
  else error()
  where
    output :: TxOut
    output = case [o | o <- outputs, scriptDatumExists o] of
      [o] -> o
      _ -> error()

{-# INLINEABLE scriptDatumExists #-}
scriptDatumExists :: TxOut -> Bool
scriptDatumExists output = case txOutDatum output of
    OutputDatum _ -> True
    OutputDatumHash _ -> True
    NoOutputDatum -> False

{-# INLINEABLE getCS #-}
getCS :: (a, b, c) -> a
getCS (a,_,_) = a

{-# INLINEABLE getTN #-}
getTN :: (a, b, c) -> b
getTN (_,b,_) = b

{-# INLINEABLE getAmount #-}
getAmount :: (a, b, c) -> c
getAmount (_,_,c) = c

{-# INLINEABLE adaCoin #-}
adaCoin :: AssetClass
adaCoin = assetClass adaSymbol adaToken

{-# INLINEABLE fromJustCustom #-}
fromJustCustom :: Maybe a -> a
fromJustCustom a = case a of
    Just b -> b
    _ -> error()
