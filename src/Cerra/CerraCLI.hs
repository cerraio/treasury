{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api hiding (Script)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Cerra.FactoryFT.OnChain
import Cerra.Staking.OnChain
import Cerra.Treasury.OnChain
import Ledger (Script)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "compile" -> do
      writePlutusScript' "Staking script" "plutus/staking_script.plutus" mkStakingScript
      writePlutusScript' "Factory policy" "plutus/factory_policy.plutus" mkFTScript
      writePlutusScript' "Treasury script" "plutus/treasury_script.plutus" mkTreasuryScript
    _ -> error "Command not supported"

writePlutusScript' :: String -> FilePath -> Script -> IO ()
writePlutusScript' _ filename scrpt =
  do
    let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
    let scriptSerial = PlutusScriptSerialised scriptSBS :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()
