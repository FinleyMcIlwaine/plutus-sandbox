{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module PlutusCore.Examples where

import Language.PlutusTx.TH (compile)
import Language.PlutusTx.Code (CompiledCode)
import Language.PlutusTx.Prelude
import Data.Text.Prettyprint.Doc

one :: CompiledCode Integer
one = $$(compile [|| (1 :: Integer) ||])
