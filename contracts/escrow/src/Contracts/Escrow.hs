{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Contracts.Escrow where

import           Language.PlutusTx
import           Language.PlutusTx.Lift
import           Language.PlutusTx.Code
import           Language.PlutusTx.Builtins
import           Language.PlutusTx.Prelude
import           Language.PlutusCore.Universe as PLC
import           Language.Plutus.Contract

type EscrowSchema =
    BlockchainActions