{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Contracts.UseCases.Vesting where

import GHC.Generics (Generic)
import Language.Plutus.Contract
  ( BlockchainActions,
    Endpoint,
    type (.\/),
  )
import qualified Language.PlutusTx as PlutusTx
import Ledger (PubKeyHash, Slot)
import Ledger.Ada (fromValue, getLovelace, lovelaceValueOf)
import Ledger.Typed.Scripts (ScriptType (..))
import Ledger.Value (Value)

type VestingSchema =
  BlockchainActions
    .\/ Endpoint "vest funds" ()
    .\/ Endpoint "retrieve funds" Value

data Vesting

instance ScriptType Vesting where
  type RedeemerType Vesting = ()
  type DatumType Vesting = ()

data VestingTranche = VestingTranche
  { vestingTrancheDate :: Slot,
    vestingTrancheAmount :: Value
  }

PlutusTx.makeLift ''VestingTranche

data VestingParams = VestingParams
  { vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner :: PubKeyHash
  }
  deriving (Generic)

{-# INLINEABLE totalAmount #-}
totalAmount :: VestingParams -> Value
totalAmount VestingParams {..} = lovelaceValueOf $ (toLovelace $ vestingTrancheAmount vestingTranche1) + (toLovelace $ vestingTrancheAmount vestingTranche2)
  where
    toLovelace = getLovelace . fromValue


