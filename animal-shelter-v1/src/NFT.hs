{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

data PolicyParam = PolicyParam
    { ppProjectPubKeyHash :: !PaymentPubKeyHash
    , ppManagerPubKeyHash :: !PaymentPubKeyHash
    } deriving (Show, Generic)

PlutusTx.makeLift ''PolicyParam
--PlutusTx.unstableMakeIsData ''PolicyParam

{-# INLINABLE params1 #-}
params1 :: PolicyParam
params1 = PolicyParam 
    { ppProjectPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
    , ppManagerPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
    }

{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParam -> () -> ScriptContext -> Bool
mkPolicy _ () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                    traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = True

    checkMintedAmount :: Bool
    checkMintedAmount = True

policy :: PolicyParam -> Scripts.MintingPolicy
policy p = mkMintingPolicyScript $
        $$(PlutusTx.compile [|| wrap ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode p
    where
        wrap mp' = Scripts.wrapMintingPolicy $ mkPolicy mp'

curSymbol :: PolicyParam -> CurrencySymbol
curSymbol p = scriptCurrencySymbol $ policy p

data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (curSymbol params1) tn 1
                lookups = Constraints.mintingPolicy (policy params1) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
