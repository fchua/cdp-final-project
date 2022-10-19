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

module AnimalShelter where

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

data ShelterRedeemer = Register | Donate
    deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''ShelterRedeemer

{-# INLINABLE params1 #-}
params1 :: PolicyParam
params1 = PolicyParam 
    { ppProjectPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
    , ppManagerPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
    }

{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParam -> ShelterRedeemer -> ScriptContext -> Bool
mkPolicy _ redeemer ctx = 
    traceIfFalse "UTxO not consumed"   hasUTxO           &&
    traceIfFalse "wrong amount minted" checkMintedAmount &&
    case redeemer of
        Register -> True
        Donate -> True
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

type NFTSchema = 
        Endpoint "mint"     NFTParams
    .\/ Endpoint "register" NFTParams
    .\/ Endpoint "donate"   NFTParams

register :: NFTParams -> Contract w NFTSchema Text ()
register np = do
    Contract.logInfo @String "Registering animal..."
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
                refTn   = TokenName $ (appendByteString "(100)" (unTokenName tn))
                usrTn   = TokenName $ (appendByteString "(222)" (unTokenName tn))
                r       = Redeemer $ PlutusTx.toBuiltinData Register
                val     = Value.singleton (curSymbol params1) refTn 1 <> Value.singleton (curSymbol params1) usrTn 1
                lookups = Constraints.mintingPolicy (policy params1) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer r val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

donate :: NFTParams -> Contract w NFTSchema Text ()
donate np = do
    Contract.logInfo @String "Sending donation..."
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (curSymbol params1) "(333) Frank" 1
                lookups = Constraints.mintingPolicy (policy params1) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)            

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (curSymbol params1) "(100) Frank" 1 <> Value.singleton (curSymbol params1) "(200) Frank" 1
                lookups = Constraints.mintingPolicy (policy params1) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = awaitPromise (mint' `select` register' `select` donate') >> endpoints
  where
    mint'     = endpoint @"mint"     mint
    register' = endpoint @"register" register
    donate'   = endpoint @"donate"   donate

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"register" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"donate" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1