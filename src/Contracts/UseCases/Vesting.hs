{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
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
import Language.PlutusTx.Prelude (AdditiveMonoid (zero))
import Ledger (PubKeyHash, Slot)
import qualified Ledger.Interval as Interval
import qualified Ledger.Slot as Slot
import Ledger.Typed.Scripts (ScriptType (..))
import Ledger.Value (Value)
import Language.PlutusTx.Monoid (Group(inv))
import Ledger.Validation (ValidatorCtx(..), TxInfo(..))
import qualified Ledger.Validation as Validation
import qualified Ledger.Value as Value
import Ledger (Validator)
import qualified Ledger.Typed.Scripts as Scripts

-- | A vesting contract supports two operations: Vesting and retrieving. Money
-- will be locked by the contract, and it will only be retrievable after some
-- specified amount of time has passed. The Vesting Schema is therefore a row
-- type that merges the predefined group of blockchain actions with those we
-- care about for our contract (vest and retrieve).
type VestingSchema =
  BlockchainActions
    .\/ Endpoint "vest funds" ()
    .\/ Endpoint "retrieve funds" Value

-- | Data type representing our vesting script.
data Vesting

instance ScriptType Vesting where
  type RedeemerType Vesting = ()
  type DatumType Vesting = ()

-- | In the vesting scheme for this contract, money will be released in two
-- tranches (parts). A smaller part will be available after an initial number
-- of slots have passed. The entire amount is available after the final date.
-- The owner of the money can retrieve any amount of funds up to the currently
-- "available" amount while remaining fundss stay locked.
data VestingTranche = VestingTranche
  { vestingTrancheDate :: Slot,
    vestingTrancheAmount :: Value
  }

-- Make the VestingTranche data type available on the blockchain.
PlutusTx.makeLift ''VestingTranche

-- | The parameters that define a two-tranche vesting scheme.
data VestingParams = VestingParams
  { vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner :: PubKeyHash
  }
  -- deriving (Generic, PlutusTx.Lift DefaultUni)
  deriving Generic

PlutusTx.makeLift ''VestingParams  

{-# INLINEABLE totalAmount #-}
-- | Retrieve the total amount vested based on the parameters
totalAmount :: VestingParams -> Value
totalAmount VestingParams {..} = vestingTrancheAmount vestingTranche1 <> vestingTrancheAmount vestingTranche2

{-# INLINEABLE availableFrom #-}
-- | The amount guaranteed to be available from a given tranche in a given slot
-- range.
availableFrom :: VestingTranche -> Slot.SlotRange -> Value
availableFrom (VestingTranche d v) range =
  -- The valid range is any slot after the one specified for the tranche
  let validRange = Interval.from d
  -- If the valid range contains the argument range (start slot of argument
  -- range is after the tranche start), then the whole amount in the tranche
  -- is available, otherwise nothing.
   in if validRange `Interval.contains` range then v else zero

-- | The amount available at a slot.
availableAt :: VestingParams -> Slot -> Value
availableAt VestingParams {..} sl =
  let f VestingTranche {..} = if sl >= vestingTrancheDate then vestingTrancheAmount else mempty
   in foldMap f [vestingTranche1, vestingTranche2]

{-# INLINABLE remainingFrom #-}
-- | The amount that has not been released from this tranche yet
remainingFrom :: VestingTranche -> Slot.SlotRange -> Value
remainingFrom t@VestingTranche{..} range = vestingTrancheAmount <> (inv $ availableFrom t range)

{-# INLINABLE validate #-}
validate :: VestingParams -> () -> () -> ValidatorCtx -> Bool
validate VestingParams{..} () () ctx@ValidatorCtx{ valCtxTxInfo=txInfo@TxInfo{ txInfoValidRange } } =
  let
    -- The amount that's actually locked by the validator script right now
    remainingActual = Validation.valueLockedBy txInfo (Validation.ownHash ctx)
    -- The sum of amounts remaining in each tranche
    remainingExpected = remainingFrom vestingTranche1 txInfoValidRange <> remainingFrom vestingTranche2 txInfoValidRange
  in remainingActual `Value.geq` remainingExpected
    -- The following condition makes transactions valid as long as they are
    -- _signed by_ the vesting scheme owner. This means they can instruct
    -- the contract to pay out to other parties and can potentially save
    -- some transactions.
    && Validation.txSignedBy txInfo vestingOwner

vestingScript :: VestingParams -> Validator
vestingScript = Scripts.validatorScript . scriptInstance


-- | 
scriptInstance :: VestingParams -> Scripts.ScriptInstance Vesting
scriptInstance vesting = Scripts.validator @Vesting
  ($$(PlutusTx.compile [|| validate ||]) `PlutusTx.applyCode` PlutusTx.liftCode vesting)
  $$(PlutusTx.compile [|| wrap ||])
  where wrap = Scripts.wrapValidator @() @()

{-
-- | A note on what's going here:
PlutusTx.compile is the following type:
        Q (TExp a) -> Q (TExp (CompiledCode PLC.DefaultUni a))

So it takes a quoted, typed, Haskell expression, and produces a corresponding
quoted, typed, Plutus Core expression.

-}