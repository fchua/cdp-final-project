{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module NFTMint
  ( printParams,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
    test
  )
where

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Data.Text                            (pack, Text)
import           Data.Void                            (Void)
import           GHC.Generics                         (Generic)
import           Ledger                               (getCardanoTxId)
import qualified Ledger.Constraints                   as Constraints
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Ledger.Value                         as Value
import           Plutus.Contract
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.Trace.Emulator                as Emulator
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Contexts            (ownCurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding (Semigroup (..), unless, (.))
import           Prelude                              (String, IO, Semigroup (..), Show (..), print, (.))
import           Text.Printf                          (printf)
import           Wallet.Emulator.Wallet               (knownWallet)

-- this should be fixed so the policy id will be the same
data NFTParams = NFTParams
    { mpProjectName :: !BuiltinByteString
    , mpProjectPubKeyHash :: !PlutusV2.PubKeyHash
    } deriving Show

PlutusTx.makeLift ''NFTParams
PlutusTx.unstableMakeIsData ''NFTParams

params1 :: NFTParams
params1 = NFTParams { mpProjectName = "Hound Haven", mpProjectPubKeyHash = PlutusV2.PubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" }

params2 :: NFTParams
params2 = NFTParams { mpProjectName = "Pawssion Project", mpProjectPubKeyHash = PlutusV2.PubKeyHash "82669eddc629c8ce5cc3cb908cec6de339281bb0a0ec111880ff0936132ac8b0" }

printParams = print $ "Params: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData params1)

data NFTRedeemer = Register | Update | Unregister | Donate
    deriving Show

PlutusTx.makeLift ''NFTRedeemer
PlutusTx.unstableMakeIsData ''NFTRedeemer

{-# INLINABLE mkPolicy #-}
mkPolicy :: NFTParams -> NFTRedeemer -> PlutusV2.ScriptContext -> Bool
mkPolicy p r ctx = traceIfFalse "wrong amount minted" checkNFTAmount &&
  case r of
    Register -> True
    Update -> True
    Unregister -> True
    Donate -> True

  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

--    hasUTxO :: Bool
--    hasUTxO = any (\i -> PlutusV2.txInInfoOutRef i == mpTxOutRef p) $ PlutusV2.txInfoInputs info

    checkNFTAmount :: Bool
    checkNFTAmount = case Value.flattenValue (PlutusV2.txInfoMint info) of
       [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == PlutusV2.TokenName "" && amt == 1
       _                -> False


{-
    As a Minting Policy
-}

policy :: NFTParams -> Scripts.MintingPolicy
policy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp
  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkPolicy mp'

{-
    As a Script
-}

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript $ policy params1

{-
    As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

{-
    As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "nft-mint-V2.plutus" Nothing serialisedScript

data RegisterParams = RegisterParams
    { rpTokenName :: !PlutusV2.TokenName
    , rpDescription :: !BuiltinByteString
    } deriving (Generic, A.ToJSON, A.FromJSON)

data UnregisterParams = UnregisterParams
    { unpTokenName :: !PlutusV2.TokenName
    , unpDescription :: !BuiltinByteString
    } deriving (Generic, A.ToJSON, A.FromJSON)

data UpdateParams = UpdateParams
    { upTokenName :: !PlutusV2.TokenName
    , upDescription :: !BuiltinByteString
    } deriving (Generic, A.ToJSON, A.FromJSON)

data DonateParams = DonateParams
    { dpTokenName :: !PlutusV2.TokenName
    , dpDescription :: !BuiltinByteString
    } deriving (Generic, A.ToJSON, A.FromJSON)

type NFTSchema =
        Endpoint "register"   RegisterParams
    .\/ Endpoint "unregister" UnregisterParams
    .\/ Endpoint "update"     UpdateParams
    .\/ Endpoint "donate"     DonateParams

register :: forall w s. RegisterParams -> Contract w s Text ()
register RegisterParams{..} = do
  let val     = Value.singleton (curSymbol params1) rpTokenName 1
      lookups = Constraints.mintingPolicy $ policy params1
      tx      = Constraints.mustMintValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ printf "Register... (%s, %s)" (show rpTokenName) (show rpDescription)

unregister :: forall w s. UnregisterParams -> Contract w s Text ()
unregister UnregisterParams{..} = do
  logInfo @String $ printf "Unregister... (%s, %s)" (show unpTokenName) (show unpDescription)

update :: forall w s. UpdateParams -> Contract w s Text ()
update UpdateParams{..} = do
  logInfo @String $ printf "Update... (%s, %s)" (show upTokenName) (show upDescription)

donate :: forall w s. DonateParams -> Contract w s Text ()
donate DonateParams{..} = do
  logInfo @String $ printf "Donate... (%s, %s)" (show dpTokenName) (show dpDescription)

endpoints :: Contract () NFTSchema Text ()
endpoints = awaitPromise (register' `select` unregister' `select` update' `select` donate') >> endpoints
  where
    register' = endpoint @"register" register
    unregister'   = endpoint @"unregister" unregister
    update' = endpoint @"update" update
    donate' = endpoint @"donate" donate

test :: IO ()
test = runEmulatorTraceIO $ do
    let projectName       = "Hound Haven"
    h <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"register" h $ RegisterParams
        { rpTokenName = "Happy"
        , rpDescription  = "The quick brown fox jumps over the lazy dog"
        }
    void $ Emulator.waitNSlots 1
