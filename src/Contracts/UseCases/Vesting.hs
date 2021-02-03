{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Contracts.UseCases.Vesting where

import           Control.Lens
import           Control.Monad                     (void, when)
import           Data.Aeson                        (FromJSON, ToJSON)
import qualified Data.Map                          as Map
import           Prelude                           (Semigroup (..))

import           GHC.Generics                      (Generic)
import           Language.Plutus.Contract          hiding (when)
import qualified Language.Plutus.Contract.Typed.Tx as Typed
import qualified Language.PlutusTx                 as PlutusTx
import           Language.PlutusTx.Prelude         hiding (Semigroup (..), fold)
import           Ledger                            (Address, PubKeyHash (..), Slot (..), Validator)
import           Ledger.Constraints                (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import           Ledger.Contexts                   (TxInfo (..), ValidatorCtx (..))
import qualified Ledger.Contexts                   as Validation
import qualified Ledger.Interval                   as Interval
import qualified Ledger.Slot                       as Slot
import qualified Ledger.Tx                         as Tx
import           Ledger.Typed.Scripts              (ScriptType (..))
import qualified Ledger.Typed.Scripts              as Scripts
import           Ledger.Value                      (Value)
import qualified Ledger.Value                      as Value
import qualified Prelude                           as Haskell

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
  } deriving (Generic)

-- Make the VestingTranche data type available on the blockchain.
PlutusTx.makeLift ''VestingTranche

-- | The parameters that define a two-tranche vesting scheme.
data VestingParams = VestingParams
  { vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner :: PubKeyHash
  }
  -- deriving (Generic, PlutusTx.Lift DefaultUni)
  deriving (Generic)

PlutusTx.makeLift ''VestingParams

{-# INLINEABLE totalAmount #-}
-- | Retrieve the total amount vested based on the parameters
totalAmount :: VestingParams -> Value
totalAmount VestingParams {vestingTranche1,vestingTranche2} = vestingTrancheAmount vestingTranche1 + vestingTrancheAmount vestingTranche2

{-# INLINEABLE availableFrom #-}
-- | The amount guaranteed to be available from a given tranche in a given slot
-- range.
availableFrom :: VestingTranche -> Slot.SlotRange -> Value
availableFrom (VestingTranche d v) range =
  -- The valid range is any slot after the one specified for the tranche
  let validRange = Interval.from d
   in -- If the valid range contains the argument range (start slot of argument
      -- range is after the tranche start), then the whole amount in the tranche
      -- is available, otherwise nothing.
      if validRange `Interval.contains` range then v else zero

-- | The amount available at a slot.
availableAt :: VestingParams -> Slot -> Value
availableAt VestingParams {vestingTranche1, vestingTranche2} sl =
  let f VestingTranche {vestingTrancheDate, vestingTrancheAmount} = if sl >= vestingTrancheDate then vestingTrancheAmount else mempty
   in foldMap f [vestingTranche1, vestingTranche2]

{-# INLINEABLE remainingFrom #-}
-- | The amount that has not been released from this tranche yet
remainingFrom :: VestingTranche -> Slot.SlotRange -> Value
remainingFrom t@VestingTranche {vestingTrancheAmount} range = vestingTrancheAmount - availableFrom t range

{-# INLINEABLE validate #-}
validate :: VestingParams -> () -> () -> ValidatorCtx -> Bool
validate VestingParams {vestingTranche1, vestingTranche2, vestingOwner} () () ctx@ValidatorCtx{valCtxTxInfo=txInfo@TxInfo{txInfoValidRange}} =
  let
    remainingActual = Validation.valueLockedBy txInfo (Validation.ownHash ctx)
    remainingExpected = remainingFrom vestingTranche1 txInfoValidRange + remainingFrom vestingTranche2 txInfoValidRange
  in remainingActual `Value.geq` remainingExpected && (Validation.txSignedBy txInfo vestingOwner)

-- | A note on what's going here:
-- PlutusTx.compile is the following type:
--         Q (TExp a) -> Q (TExp (CompiledCode PLC.DefaultUni a))

-- So it takes a quoted, typed, Haskell expression, and produces a corresponding
-- quoted, typed, Plutus Core expression.

-- We then apply the generated PLC validate function to the vesting parameters
-- (which have also been converted into their PLC equivalent)

-- So, the value:
--    ($$(PlutusTx.compile [||validate||]) `PlutusTx.applyCode` PlutusTx.liftCode vesting)
-- has type:
--    CompiledCode DefaultUni (() -> () -> ValidatorCtx -> Bool)

-- Note that the type () -> () -> ValidatorCtx -> Bool is the type of a validator that has
-- unit type datum and redeemer.

-- Then we take that generated validator, and pass it to the Ledger.Typed.Scripts.validator
-- function, along with a function to wrap that validator, to ultimately get the script
-- instance.



scriptInstance :: VestingParams -> Scripts.ScriptInstance Vesting
scriptInstance vesting =
  Scripts.validator @Vesting
    ($$(PlutusTx.compile [||validate||]) `PlutusTx.applyCode` PlutusTx.liftCode vesting)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @()

-- | The Validator script from the vesting validator instance
vestingScript :: VestingParams -> Validator
vestingScript = Scripts.validatorScript . scriptInstance

-- | The script address of the validator instance
contractAddress :: VestingParams -> Ledger.Address
contractAddress = Scripts.scriptAddress . scriptInstance

data VestingError =
  VContractError ContractError
  | InsufficientFundsError Value Value Value
  deriving (Haskell.Eq, Show)
