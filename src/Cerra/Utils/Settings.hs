{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Utils.Settings
  ( stakingTokenName,
    cerraAssetClass,
    oracleFTAssetClass,
    minimumLovelaceStep
  )
where

import Ledger (AssetClass)
import Data.Maybe (fromJust)
import Plutus.V2.Ledger.Api (TokenName, CurrencySymbol)
import Ledger.Value (assetClass, currencySymbol, tokenName)
import Text.Hex (Text, decodeHex)
import PlutusTx.Prelude(($), Integer)

--import Cerra.Utils.Debug (debugError)

{-# INLINEABLE stakingTokenName #-}
stakingTokenName :: TokenName
stakingTokenName = tokenName $ fromJust $ decodeHex "5354414b494e47"

cerraSymbol :: Text.Hex.Text
cerraSymbol = "46f987f7ed1886ba771b077c3ed5bbf3df158f54e0e3fa88d3d1e46e"

{-# INLINEABLE cerraName #-}
cerraName :: TokenName
cerraName = tokenName $ fromJust $ decodeHex "744345525241"

{-# INLINEABLE cerraCurrencySymbol #-}
cerraCurrencySymbol :: CurrencySymbol
cerraCurrencySymbol = currencySymbol $ fromJust $ decodeHex cerraSymbol

{-# INLINEABLE cerraAssetClass #-}
cerraAssetClass :: AssetClass
cerraAssetClass = assetClass cerraCurrencySymbol cerraName

oracleFT :: Text.Hex.Text
oracleFT = "5dbf00aff594765201981c9a113869914ac7937391045a1dc3d7360d"

{-# INLINEABLE oracleFTName #-}
oracleFTName :: TokenName
oracleFTName = tokenName $ fromJust $ decodeHex "5354414b494e474c4943454e5345"

{-# INLINEABLE oracleFTSymbol #-}
oracleFTSymbol :: CurrencySymbol
oracleFTSymbol = currencySymbol $ fromJust $ decodeHex oracleFT

{-# INLINEABLE oracleFTAssetClass #-}
oracleFTAssetClass :: AssetClass
oracleFTAssetClass = assetClass oracleFTSymbol oracleFTName

{-# INLINEABLE minimumLovelaceStep #-}
minimumLovelaceStep :: Integer
minimumLovelaceStep = 2_000_000
