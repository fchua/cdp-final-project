{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Donation where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless)
import           Prelude              (IO, Semigroup (..), Show (..), String, Eq)
import           Schema               (ToSchema)
import           Text.Printf          (printf)

data ShelterDatum = ShelterDatum
    { sdTreasury :: !PaymentPubKeyHash
    , sdManager :: !PaymentPubKeyHash
    } deriving Show

PlutusTx.unstableMakeIsData ''ShelterDatum

data ShelterRedeemer = Mint | Burn | Donate
    deriving (Generic, FromJSON, ToJSON, Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''ShelterRedeemer

{-# INLINABLE mkPolicy #-}
mkPolicy :: ShelterRedeemer -> ScriptContext -> Bool 
mkPolicy redeemer ctx = True

{-# INLINABLE mkValidator #-}
mkValidator :: ShelterDatum -> ShelterRedeemer -> ScriptContext -> Bool
mkValidator datum action ctx = traceIfFalse "" signedByManager
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByManager :: Bool
        signedByManager = txSignedBy info $ unPaymentPubKeyHash $ sdManager datum

data Shelter
instance Scripts.ValidatorTypes Shelter where
    type instance DatumType Shelter = ShelterDatum
    type instance RedeemerType Shelter = ShelterRedeemer

typedValidator :: Scripts.TypedValidator Shelter
typedValidator = Scripts.mkTypedValidator @Shelter
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ShelterDatum @ShelterRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data MintParams = MintParams 
    { mpTokenName :: !TokenName
    , mpCurrency  :: !CurrencySymbol
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BurnParams = BurnParams
    { bpTokenName :: !TokenName
    , bpCurrency  :: !CurrencySymbol
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data DonateParams = DonateParams
    { dpTokenName :: !TokenName
    , dpCurrency  :: !CurrencySymbol
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
    
mintAnimal :: forall w s. MintParams -> Contract w s Text ()
mintAnimal mp = do
    logInfo @String $ "Mint..."

burnAnimal :: forall w s. BurnParams -> Contract w s Text ()
burnAnimal mp = do
    logInfo @String $ "Burn..."

donateAnimal :: forall w s. DonateParams -> Contract w s Text ()
donateAnimal mp = do
    logInfo @String $ "Donate..."

type ShelterSchema =
            Endpoint "mint" MintParams
        .\/ Endpoint "burn" BurnParams
        .\/ Endpoint "donate" DonateParams

endpoints :: Contract () ShelterSchema Text ()
endpoints = awaitPromise (mint `select` burn `select` donate) >> endpoints
    where
        mint = endpoint @"mint" mintAnimal
        burn = endpoint @"burn" burnAnimal
        donate = endpoint @"donate" donateAnimal

mkSchemaDefinitions ''ShelterSchema

mkKnownCurrencies []

-- no. of writes = need more data
-- performance if using Kafka
-- theoritical volume of data and writes
