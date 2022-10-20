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

--- ON-CHAIN ---

-- This is used to parameterized the minting policy so we can control who is allowed to mint
data PolicyParam = PolicyParam
    { ppTreasuryPubKeyHash :: !PaymentPubKeyHash -- treasury public key hash
    , ppManagerPubKeyHash  :: !PaymentPubKeyHash -- manager public key hash
    , ppProjectName        :: !BuiltinByteString -- additional identifier
    } deriving (Show, Generic)

PlutusTx.makeLift ''PolicyParam
--PlutusTx.unstableMakeIsData ''PolicyParam

-- Minting policy actions
-- We can also add parameters in the future for additional controls
data ShelterRedeemer = Register | Donate | Unregister
    deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''ShelterRedeemer

--params1 :: PolicyParam
--params1 = PolicyParam 
--    { ppTreasuryPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
--    , ppManagerPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
--    }

-- traceIfFalse "wrong amount minted" checkMintedAmount &&

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html#t:ScriptContext

{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParam -> ShelterRedeemer -> ScriptContext -> Bool
mkPolicy pp redeemer ctx = 
    case redeemer of
        Register -> True
        -- must mint 2 tokens (reference and user token)
        -- reference token must have a label (100)
        -- user token must have a label (222)
        -- only the manager is allowed to mint the token
        -- the tokens must be sent to the script address
        Donate -> True
        -- must transfer minimum amount to treasury
        -- must mint only 1 token
        -- minted token name must be same as the reference token and has a label (333)
        -- reference token must be valid and owned by the script
        -- any one can mint
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputs :: [TxInInfo]
    inputs = txInfoInputs info

    outputs :: [TxOut]
    outputs = txInfoOutputs info

    minted :: Value
    minted = txInfoMint info

    signedByManager :: Bool
    signedByManager = txSignedBy info (ppManagerPubKeyHash pp)

    validReferenceAndUserTokens :: Bool
    validReferenceAndUserTokens = True

    hasUTxO :: Bool
    hasUTxO = True

    checkMintedAmount :: Bool
    checkMintedAmount = True

-- function to generate the parameterized minting policy script
policy :: PolicyParam -> Scripts.MintingPolicy
policy p = mkMintingPolicyScript $
        $$(PlutusTx.compile [|| wrap ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode p
    where
        wrap mp' = Scripts.wrapMintingPolicy $ mkPolicy mp'

-- function to generate the currency symbol using the parametarized minting policy script
curSymbol :: PolicyParam -> CurrencySymbol
curSymbol p = scriptCurrencySymbol $ policy p

--- OFF-CHAIN ---

data RegisterParams = RegisterParams
    { rpToken       :: !TokenName -- base token name, how to guarantee uniqueness?
    , rpAddress     :: !Address -- can we auto-detect this inside the Contract?
    , rpDescription :: !BuiltinByteString
    , rpPolicyParam :: !PolicyParam
    } deriving (Generic, FromJSON, ToJSON, Show)

data DonateParams = DonateParams
    { dpToken       :: !TokenName
    , dpAddress     :: !Address
    , dpPolicyParam :: !PolicyParam
    , npDonation    :: !Integer
    } deriving (Generic, FromJSON, ToJSON, Show)

type ShelterSchema = 
        Endpoint "register"   RegisterParams
    .\/ Endpoint "donate"     DonateParams
    .\/ Endpoint "unregister" RegisterParams

register :: RegisterParams -> Contract w ShelterSchema Text ()
register rp = do
    Contract.logInfo @String "Registering animal..."
    utxos <- utxosAt $ rpAddress rp -- get utxos from the wallet
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do -- at least 1 utxo found
            let tn      = rpToken rp
                pp      = rpPolicyParam rp -- policy parameter
                refTn   = TokenName $ (appendByteString "(100)" (unTokenName tn))
                usrTn   = TokenName $ (appendByteString "(222)" (unTokenName tn))
                r       = Redeemer $ PlutusTx.toBuiltinData Register
                mintVal = Value.singleton (curSymbol pp) refTn 1 <>
                          Value.singleton (curSymbol pp) usrTn 1
                lookups = Constraints.mintingPolicy (policy pp) <>
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer r mintVal <>
                          Constraints.mustPayToTheScript () mintVal <> -- is this the way to send the tokens to the script after minting
                          Constraints.mustSpendPubKeyOutput oref -- not sure if this is needed, where will this go?
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

unregister :: RegisterParams -> Contract w ShelterSchema Text ()
unregister rp = do
    Contract.logInfo @String "Unregistering animal..."
    utxos <- utxosAt $ rpAddress rp -- get utxos from the wallet
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do -- at least 1 utxo found
            let tn      = rpToken rp
                pp      = rpPolicyParam rp -- policy parameter
                refTn   = TokenName $ (appendByteString "(100)" (unTokenName tn))
                usrTn   = TokenName $ (appendByteString "(222)" (unTokenName tn))
                r       = Redeemer $ PlutusTx.toBuiltinData Register
                mintVal = Value.singleton (curSymbol pp) refTn (- 1) <>
                          Value.singleton (curSymbol pp) usrTn (- 1)
                lookups = Constraints.mintingPolicy (policy pp) <>
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer r mintVal <>
                          Constraints.mustSpendPubKeyOutput oref -- not sure if this is needed, where will this go?
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

donate :: DonateParams -> Contract w ShelterSchema Text ()
donate np = do
    Contract.logInfo @String "Sending donation..."
    utxos <- utxosAt $ dpAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn          = dpToken np
                pp          = dpPolicyParam np
                donTn       = TokenName $ (appendByteString "(333)" (unTokenName tn))
                tPkh        = ppTreasuryPubKeyHash pp
                donation    = npDonation pp
                mintVal     = Value.singleton (curSymbol pp) donTn 1 -- mint donation token
                donationVal = (Ada.lovelaceValueOf donation)
                lookups     = Constraints.mintingPolicy (policy pp) <>
                              Constraints.unspentOutputs utxos
                tx          = Constraints.mustMintValue mintVal <>
                              Constraints.mustPayToPubKey tPkh donationVal -- send donation to treasury wallet
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () ShelterSchema Text ()
endpoints = awaitPromise (register' `select` donate' `select` unregister') >> endpoints
  where
    register'   = endpoint @"register"   register
    donate'     = endpoint @"donate"     donate
    unregister' = endpoint @"unregister" unregister

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1       = knownWallet 1 -- treasury wallet
        w2       = knownWallet 2 -- manager wallet
        w3       = knownWallet 3 -- donor
        w4       = knownWallet 4 -- donor
        w5       = knownWallet 5 -- donor
        pkhT     = mockWalletPaymentPubKeyHash w1
        pkhM     = mockWalletPaymentPubKeyHash w2
        paramPol = PolicyParam
                 { ppTreasuryPubKeyHash = pkhT
                 , ppManagerPubKeyHash = pkhM
                 , ppProjectName = "Hound Haven"
                 }

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    h3 <- activateContractWallet w3 endpoints
    h4 <- activateContractWallet w4 endpoints
    h5 <- activateContractWallet w5 endpoints

    callEndpoint @"register" h2 $ RegisterParams
        { rpToken       = tn
        , rpAddress     = mockWalletAddress w2
        , rpDescription = "Female/White"
        , rpPolicyParam = paramPol
        }

    void $ Emulator.waitNSlots 2
    
    callEndpoint @"donate" h3 $ DonateParams
        { dpToken       = tn
        , dpAddress     = mockWalletAddress w3
        , dpPolicyParam = paramPol
        }
    
    void $ Emulator.waitNSlots 2

    callEndpoint @"donate" h4 $ DonateParams
        { dpToken       = tn
        , dpAddress     = mockWalletAddress w4
        , dpPolicyParam = paramPol
        }

    void $ Emulator.waitNSlots 2

    callEndpoint @"unregister" h2 $ RegisterParams
        { rpToken       = tn
        , rpAddress     = mockWalletAddress w2
        , rpDescription = "Female/White"
        , rpPolicyParam = paramPol
        }
