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
import qualified PlutusTx.Builtins      as BI
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Plutus.V1.Ledger.Api   as V1
import           Ledger.Ada             as Ada
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- [Quick Guide]
-- (100)TokenName -> reference token (NFT) - locked in the script
-- (222)TokenName -> user token (NFT) - owned by the manager
-- (333)TokenName -> user token (FT) - donation tokens
-- Treasury -> where ADA donations must be sent
-- Manager -> the only wallet who can mint/burn NFTs

-- [Working Features]
-- Checking of minimum amount of ADA donation in order to mint
-- Checking if donation was sent to treasury address
-- Only manager wallet is allowed to mint reference token (100) and user token (222)
-- Donor wallets allowed to mint user token (333)
-- Only 1 user token (333) can be minted at a time

-- [Current Limitations]
-- Reference token (100) and user token (222) not yet locked to the script address
-- No validation against the name
-- Datum not yet used
-- (100)TokenName not utilizing the datum for anything

minDonation :: Integer
minDonation = 5000000 -- 5 ADA

--- ON-CHAIN ---

-- This is used to parameterized the minting policy so we can control who is allowed to mint
data PolicyParam = PolicyParam
    { ppTreasuryPubKeyHash :: !PaymentPubKeyHash -- treasury public key hash
    , ppManagerPubKeyHash  :: !PaymentPubKeyHash -- manager public key hash
    , ppProjectName        :: !BuiltinByteString -- additional identifier
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''PolicyParam
--PlutusTx.unstableMakeIsData ''PolicyParam -- Used by Validator not MintingPolicy

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/Ledger.html

-- Minting policy actions
-- We can also add parameters in the future for additional controls
data ShelterRedeemer = Register TokenName | Donate TokenName | Unregister TokenName
    deriving (Show, Generic)
PlutusTx.unstableMakeIsData ''ShelterRedeemer

data ShelterDatum = ShelterDatum
    { sdManagerPubKeyHash  :: !PaymentPubKeyHash -- manager public key hash
    , sdProjectName        :: !BuiltinByteString -- additional identifier
    } deriving (Show, Generic, FromJSON, ToJSON)
PlutusTx.unstableMakeIsData ''ShelterDatum -- Used by Validator not MintingPolicy    

data AnimalDatum = AnimalDatum Integer
    deriving Show
PlutusTx.unstableMakeIsData ''AnimalDatum

-- NOT SURE if this is really necessary since we are using Minting Policy not Validator
data Shelter
instance Scripts.ValidatorTypes Shelter where
    type instance DatumType Shelter = ShelterDatum
    type instance RedeemerType Shelter = ShelterRedeemer

--params1 :: PolicyParam
--params1 = PolicyParam 
--    { ppTreasuryPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
--    , ppManagerPubKeyHash = PaymentPubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" 
--    }

-- traceIfFalse "wrong amount minted" checkMintedAmount &&

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html

-- Returns the reference token name
{-# INLINABLE toReferenceToken #-}
toReferenceToken :: TokenName -> TokenName
toReferenceToken tn = TokenName $ appendByteString "(100)" (unTokenName tn)

-- Returns the user token name
{-# INLINABLE toUserToken #-}
toUserToken :: TokenName -> TokenName
toUserToken tn = TokenName $ appendByteString "(222)" (unTokenName tn)

-- Returns the donation token name
{-# INLINABLE toDonationToken #-}
toDonationToken :: TokenName -> TokenName
toDonationToken tn = TokenName $ appendByteString "(333)" (unTokenName tn)

-- Returns the CurrencySymbol
{-# INLINABLE extractSym #-}
extractSym :: (CurrencySymbol, TokenName, Integer) -> CurrencySymbol
extractSym (s, _, _) = s

-- Returns the TokenName
{-# INLINABLE extractTok #-}
extractTok :: (CurrencySymbol, TokenName, Integer) -> TokenName
extractTok (_, t, _) = t

-- Returns the Integer
{-# INLINABLE extractInt #-}
extractInt :: (CurrencySymbol, TokenName, Integer) -> Integer
extractInt (_, _, i) = i

{-# INLINEABLE mkValidator #-}
mkValidator :: ShelterDatum -> ShelterRedeemer -> ScriptContext -> Bool
mkValidator _ _ _ = traceIfFalse "Spending not allowed" False

typedShelterValidator :: Scripts.TypedValidator Shelter
typedShelterValidator = Scripts.mkTypedValidator @Shelter
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ShelterDatum @ShelterRedeemer

shelterValidator :: Validator
shelterValidator = Scripts.validatorScript typedShelterValidator

shelterHash :: Ledger.ValidatorHash
shelterHash = Scripts.validatorHash typedShelterValidator

shelterAddress :: Ledger.Address
shelterAddress = scriptHashAddress shelterHash

{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParam -> ShelterRedeemer -> ScriptContext -> Bool
mkPolicy pp redeemer ctx = 
  case redeemer of
        Register tn -> traceIfFalse "Wallet not authorized to mint tokens" signedByManager &&
                       traceIfFalse "Reference (100) token not minted" (checkReferenceToken tn) &&
                       traceIfFalse "User (222) token not minted" (checkUserToken tn)
        -- must mint 2 tokens (reference and user token) - DONE
        -- reference token must have a label (100) - DONE
        -- user token must have a label (222) - DONE
        -- only the manager is allowed to mint the token - DONE
        -- the tokens must be sent to the script address
        Donate tn -> traceIfFalse "Minimum donation not met" minDonationReceived &&
                     traceIfFalse "Donation token not minted" (checkDonationToken tn)
        -- must transfer minimum amount to treasury
        -- must mint only 1 token
        -- minted token name must be same as the reference token and has a label (333)
        -- reference token must be valid and owned by the script
        -- any one can mint
        Unregister tn -> traceIfFalse "Wallet not authorized to burn tokens" signedByManager
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputs :: [TxInInfo]
    inputs = txInfoInputs info

    outputs :: [TxOut]
    outputs = txInfoOutputs info

    minted :: Value
    minted = txInfoMint info

    ownSym :: CurrencySymbol
    ownSym = ownCurrencySymbol ctx

    treasuryAddress :: Address
    treasuryAddress = pubKeyHashAddress (ppTreasuryPubKeyHash pp) Nothing

    managerAddress :: Address
    managerAddress = pubKeyHashAddress (ppManagerPubKeyHash pp) Nothing

    -- True if signed by the manager wallet
    signedByManager :: Bool
    signedByManager = txSignedBy info (unPaymentPubKeyHash $ ppManagerPubKeyHash pp)

    -- True if there's only 1 donation token minted
    checkDonationToken :: TokenName -> Bool
    checkDonationToken tn = cnt == 1
        where
            cnt = sum $ fmap extractInt (filter (\fv -> isDonationToken tn fv) (flattenValue minted))

    -- True if there's only 1 reference token minted
    checkReferenceToken :: TokenName -> Bool
    checkReferenceToken tn = cnt == 1
        where
            cnt = sum $ fmap extractInt (filter (\fv -> isReferenceToken tn fv) (flattenValue minted))

    -- True if there's only 1 user token minted
    checkUserToken :: TokenName -> Bool
    checkUserToken tn = cnt == 1
        where
            cnt = sum $ fmap extractInt (filter (\fv -> isUserToken tn fv) (flattenValue minted))

    -- True if the token name has a prefix (333)
    isDonationToken :: TokenName -> (CurrencySymbol, TokenName, Integer) -> Bool
    isDonationToken tn fv = (extractSym fv) == ownSym && (extractTok fv) == (toDonationToken tn)

    -- True if the token name has a prefix (100)
    isReferenceToken :: TokenName -> (CurrencySymbol, TokenName, Integer) -> Bool
    isReferenceToken tn fv = (extractSym fv) == ownSym && (extractTok fv) == (toReferenceToken tn)

    -- True if the token name has a prefix (222)
    isUserToken :: TokenName -> (CurrencySymbol, TokenName, Integer) -> Bool
    isUserToken tn fv = (extractSym fv) == ownSym && (extractTok fv) == (toUserToken tn)

    -- get values sent to treasury address
    donationOutputs :: [Value]
    donationOutputs = [ (txOutValue o) | o <- outputs, (txOutAddress o) == treasuryAddress ]

    -- flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)] 
    -- flatten values to triple
    donationAmount :: [ (CurrencySymbol, TokenName, Integer) ]
    donationAmount = flattenValue $ mconcat donationOutputs

    -- filter ADA only
    donatedAda :: [ (CurrencySymbol, TokenName, Integer) ]
    donatedAda = [ a | a <- donationAmount, (extractSym a) == Ada.adaSymbol ]

    -- get total Integer
    totalAda :: Integer
    totalAda = sum $ fmap extractInt donatedAda

    -- True if minimum ADA donation was sent
    minDonationReceived :: Bool
    minDonationReceived = totalAda >= minDonation

-- Generate parameterized minting policy
policy :: PolicyParam -> Scripts.MintingPolicy
policy p = mkMintingPolicyScript $
        $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode p
    where
        wrap mp' = Scripts.wrapMintingPolicy $ mkPolicy mp'

-- Generate policy hash - but don't know how to use this yet
policyHash :: Scripts.MintingPolicy -> MintingPolicyHash
policyHash mp = mintingPolicyHash mp

-- function to generate the currency symbol using the parametarized minting policy script
toCurrencySymbol :: PolicyParam -> CurrencySymbol
toCurrencySymbol p = scriptCurrencySymbol $ policy p

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
    , dpDonation    :: !Integer
    } deriving (Generic, FromJSON, ToJSON, Show)

type ShelterSchema = 
        Endpoint "register"   RegisterParams
    .\/ Endpoint "donate"     DonateParams
    .\/ Endpoint "unregister" RegisterParams

register :: RegisterParams -> Contract w ShelterSchema Text ()
register rp = do
    logInfo @String "Registering animal..."
    utxos <- utxosAt $ rpAddress rp -- get utxos from the wallet
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do -- at least 1 utxo found
            let tn      = rpToken rp
                pp      = rpPolicyParam rp -- policy parameter
                refTn   = toReferenceToken tn
                usrTn   = toUserToken tn
                r       = Redeemer $ PlutusTx.toBuiltinData (Register tn)
                mintVal = Value.singleton (toCurrencySymbol pp) refTn 1 <>
                          Value.singleton (toCurrencySymbol pp) usrTn 1
                lookups = Constraints.mintingPolicy (policy pp) <>
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer r mintVal <>
                          Constraints.mustIncludeDatum (Datum $ BI.mkI 1) <>
                          --Constraints.mustPayToTheScript (AnimalDatum 1) mintVal <> -- is this the way to send the tokens to the script after minting
                          Constraints.mustSpendPubKeyOutput oref -- not sure if this is needed, where will this go?
            --ledgerTx <- submitTxConstraintsWith @Shelter lookups tx
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ printf "Minted %s" (show mintVal)

unregister :: RegisterParams -> Contract w ShelterSchema Text ()
unregister rp = do
    logInfo @String "Unregistering animal..."
    utxos <- utxosAt $ rpAddress rp -- get utxos from the wallet
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do -- at least 1 utxo found
            let tn      = rpToken rp
                pp      = rpPolicyParam rp -- policy parameter
                refTn   = toReferenceToken tn
                usrTn   = toUserToken tn
                r       = Redeemer $ PlutusTx.toBuiltinData (Unregister tn)
                mintVal = Value.singleton (toCurrencySymbol pp) refTn (- 1) <>
                          Value.singleton (toCurrencySymbol pp) usrTn (- 1)
                lookups = Constraints.mintingPolicy (policy pp) <>
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer r mintVal <>
                          Constraints.mustSpendPubKeyOutput oref -- not sure if this is needed, where will this go?
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ printf "Burned %s" (show mintVal)

donate :: DonateParams -> Contract w ShelterSchema Text ()
donate dp = do
    logInfo @String "Sending donation..."
    utxos <- utxosAt $ dpAddress dp
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn          = dpToken dp
                pp          = dpPolicyParam dp
                donTn       = toDonationToken tn
                r           = Redeemer $ PlutusTx.toBuiltinData (Donate tn)
                tPkh        = ppTreasuryPubKeyHash pp
                donation    = dpDonation dp
                mintVal     = Value.singleton (toCurrencySymbol pp) donTn 1 -- mint donation token
                donationVal = (Ada.lovelaceValueOf donation)
                lookups     = Constraints.mintingPolicy (policy pp) <>
                              Constraints.unspentOutputs utxos
                tx          = Constraints.mustMintValueWithRedeemer r mintVal <>
                              Constraints.mustPayToPubKey tPkh donationVal -- send donation to treasury wallet
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ printf "Minted %s" (show mintVal)

endpoints :: Contract () ShelterSchema Text ()
endpoints = awaitPromise (register' `select` donate' `select` unregister') >> endpoints
  where
    register'   = endpoint @"register"   register
    donate'     = endpoint @"donate"     donate
    unregister' = endpoint @"unregister" unregister

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "Blackie the Maligator"
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
        , dpDonation    = 5000000
        }
    
    void $ Emulator.waitNSlots 2

    callEndpoint @"donate" h4 $ DonateParams
        { dpToken       = tn
        , dpAddress     = mockWalletAddress w4
        , dpPolicyParam = paramPol
        , dpDonation    = 4999999
        }

    void $ Emulator.waitNSlots 2

    callEndpoint @"donate" h5 $ DonateParams
        { dpToken       = tn
        , dpAddress     = mockWalletAddress w5
        , dpPolicyParam = paramPol
        , dpDonation    = 10000000
        }

    void $ Emulator.waitNSlots 2
    
    callEndpoint @"donate" h3 $ DonateParams
        { dpToken       = tn
        , dpAddress     = mockWalletAddress w3
        , dpPolicyParam = paramPol
        , dpDonation    = 5000000
        }

    void $ Emulator.waitNSlots 1

    -- callEndpoint @"unregister" h2 $ RegisterParams
    --     { rpToken       = tn
    --     , rpAddress     = mockWalletAddress w2
    --     , rpDescription = "Female/White"
    --     , rpPolicyParam = paramPol
    --     }
