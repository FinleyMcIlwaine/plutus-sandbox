{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Contracts.Vesting
    (type VestingSchema)
where

import Language.Plutus.Contract
import Ledger.Value (Value)

-- Schemas are Row types built from BlockchainActions
type VestingSchema =
    BlockchainActions
        .\/ Endpoint "vest funds" ()
        .\/ Endpoint "retrieve funds" Value


