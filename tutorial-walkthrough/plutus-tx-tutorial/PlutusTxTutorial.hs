{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTxTutorial where

import Language.PlutusTx
import Language.PlutusTx.Lift
import Language.PlutusTx.Code
import Language.PlutusTx.Builtins
import Language.PlutusTx.Prelude
import Language.PlutusCore.Universe as PLC
import Language.PlutusCore.Evaluation.Machine.Ck
import Language.PlutusCore.Core
import Data.Text.Prettyprint.Doc

-- * 3.3 * Writing basic PlutusTx programs

-- One of the simplest programs that we can write:
-- Just evaluates to the integer 1
integerOne :: CompiledCode PLC.DefaultUni Integer
integerOne = $$(compile
    [|| (1 :: Integer) ||])

-- Slightly more complex, the identity function on
-- integers
integerIdentity :: CompiledCode PLC.DefaultUni (Integer -> Integer)
integerIdentity = $$(compile [|| \(x :: Integer) -> x ||])


-- * 3.4 * Functions and datatypes

-- A couple of basic inlinable functions
{-# INLINABLE plusOne #-}
plusOne :: Integer -> Integer
plusOne x = addInteger x 1

{-# INLINABLE myProgram #-}
myProgram :: Integer
myProgram =
    let
        plusOneLocal :: Integer -> Integer
        plusOneLocal x = addInteger x 1

        localTwo = plusOneLocal 1
        externalTwo = plusOne 1
    in addInteger localTwo externalTwo

-- Compiling the functions to PlutusCore using the PlutusTx compiler
functions :: CompiledCode PLC.DefaultUni Integer
functions = $$(compile [|| myProgram ||])
