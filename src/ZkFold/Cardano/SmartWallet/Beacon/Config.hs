module ZkFold.Cardano.SmartWallet.Beacon.Config (
  Config (..),
  coreConfigFromConfig,
  signingKeyFromConfig,
  simpleScriptFromConfig,
  tokenAddressFromConfig,
  configOptionalFPIO,
) where

import Data.Aeson (
  eitherDecodeFileStrict,
  eitherDecodeStrict,
 )
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (..))
import Data.Text
import Data.Text.IO qualified as TIO
import Data.Word (Word32)
import Data.Yaml qualified as Yaml
import Deriving.Aeson
import GeniusYield.GYConfig (Confidential (..), GYCoreConfig (..), GYCoreProviderInfo (..))
import GeniusYield.Imports (throwIO, (&))
import GeniusYield.Types
import System.Envy
import System.FilePath (takeExtension)

-- | Data type for deserializing input config parameters
data RawConfig = RawConfig
  { rcMaestroApiKeyPath ∷ !(Maybe FilePath)
  -- ^ Path to the file containing the core provider
  , rcNetworkId ∷ !GYNetworkId
  , rcLogging ∷ ![GYLogScribeConfig]
  , rcFundMnemonicPath ∷ !FilePath
  , rcFundAccIx ∷ !(Maybe Word32)
  , rcFundAddrIx ∷ !(Maybe Word32)
  , rcOtherSignatories ∷ ![GYPaymentKeyHash]
  , rcRequiredSignatures ∷ !Natural
  , rcTokenName ∷ !GYTokenName
  , rcPolicyId ∷ !GYMintingPolicyId
  , rcUpdateInterval ∷ !(Maybe Natural)
  , rcTokensToMint ∷ Natural
  , rcTurboSubmit ∷ !(Maybe Bool)
  }
  deriving stock Generic
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "rc", LowerFirst]] RawConfig

instance FromEnv RawConfig where
  fromEnv _ = forceFromJsonOrYaml <$> env "BEACON_CONFIG"

newtype MaestroApiKey = MaestroApiKey {unwrapMAK ∷ Text}
  deriving stock Generic
  deriving newtype FromJSON

instance FromEnv MaestroApiKey where
  fromEnv _ = forceFromJsonOrYaml <$> env "MAESTRO_API_KEY"

forceFromJsonOrYaml ∷ FromJSON a ⇒ String → a
forceFromJsonOrYaml s =
  let bs = fromString s
      parseResults = eitherDecodeStrict bs :| [first show $ Yaml.decodeEither' bs]
   in go parseResults
 where
  go (x :| []) = case x of
    Left e → error e
    Right a → a
  go (x :| y : ys) = case x of
    Left _ → go (y :| ys)
    Right a → a

eitherDecodeFileStrictJsonOrYaml ∷ FromJSON a ⇒ FilePath → IO (Either String a)
eitherDecodeFileStrictJsonOrYaml fp =
  case takeExtension fp of
    ".json" → eitherDecodeFileStrict fp
    ".yaml" → first show <$> Yaml.decodeFileEither fp
    _ → throwIO $ userError "Only .json or .yaml extensions are supported for configuration."

eitherDecodeFileText ∷ Coercible Text a ⇒ FilePath → IO (Either String a)
eitherDecodeFileText fp = pure . coerce <$> TIO.readFile fp

data Config = Config
  { cCoreProvider ∷ !GYCoreProviderInfo
  , cNetworkId ∷ !GYNetworkId
  , cLogging ∷ ![GYLogScribeConfig]
  , cFundMnemonic ∷ !Mnemonic
  -- ^ Mnemonic (seed phrase) to fund the transaction. It is also one of the signatories.
  , cFundAccIx ∷ !(Maybe Word32)
  -- ^ Account index.
  , cFundAddrIx ∷ !(Maybe Word32)
  -- ^ Payment address index.
  , cOtherSignatories ∷ ![GYPaymentKeyHash]
  -- ^ Other parties that may sign the update transaction, besides the funding wallet.
  , cRequiredSignatures ∷ !Natural
  , -- % The number of signatures required for the update transaction.
    cTokenName ∷ !GYTokenName
  -- ^ Beacon token name
  , cPolicyId ∷ !GYMintingPolicyId
  -- ^ Beacon token policy id
  , cUpdateInterval ∷ !(Maybe Natural)
  -- ^ Interval in seconds after which the manager should try to update the beacon token. If not provided, 3600 seconds (i.e., interval of an hour) is used.
  , cTokensToMint ∷ Natural
  }
  deriving stock Generic

configOptionalFPIO ∷ Maybe FilePath → IO Config
configOptionalFPIO mfp = do
  e ← maybe decodeEnv eitherDecodeFileStrictJsonOrYaml mfp
  either' (throwIO . userError) e $ \RawConfig {..} → do
    eMaestroApiKey ← maybe decodeEnv eitherDecodeFileText rcMaestroApiKeyPath
    eFundMnemonic ← eitherDecodeFileStrictJsonOrYaml rcFundMnemonicPath
    either
      (throwIO . userError)
      pure
      ( lift eMaestroApiKey eFundMnemonic $
          \(MaestroApiKey maestroApiKey) fundMnemonic →
            Config
              { cCoreProvider =
                  GYMaestro
                    { cpiMaestroToken = Confidential maestroApiKey
                    , cpiTurboSubmit = rcTurboSubmit
                    }
              , cNetworkId = rcNetworkId
              , cLogging = rcLogging
              , cFundMnemonic = fundMnemonic
              , cFundAccIx = rcFundAccIx
              , cFundAddrIx = rcFundAddrIx
              , cOtherSignatories = rcOtherSignatories
              , cRequiredSignatures = rcRequiredSignatures
              , cTokenName = rcTokenName
              , cPolicyId = rcPolicyId
              , cUpdateInterval = rcUpdateInterval
              , cTokensToMint = rcTokensToMint
              }
      )
 where
  either' l e r = either l r e
  lift a b f = f <$> a <*> b

coreConfigFromConfig ∷ Config → GYCoreConfig
coreConfigFromConfig Config {..} =
  GYCoreConfig
    { cfgCoreProvider = cCoreProvider
    , cfgNetworkId = cNetworkId
    , cfgLogging = cLogging
    , cfgLogTiming = Nothing
    }

signingKeyFromConfig ∷ Config → Maybe (GYSomePaymentSigningKey, GYAddress)
signingKeyFromConfig Config {..} = case wk' of
  Left _ → Nothing
  Right wk → Just (AGYExtendedPaymentSigningKey (walletKeysToExtendedPaymentSigningKey wk), walletKeysToAddress wk cNetworkId)
 where
  wk' = walletKeysFromMnemonicIndexed cFundMnemonic (fromMaybe 0 cFundAccIx) (fromMaybe 0 cFundAddrIx)

simpleScriptFromConfig ∷ Config → GYSimpleScript
simpleScriptFromConfig config@Config {..} = script
 where
  script ∷ GYSimpleScript
  script = RequireMOf (fromIntegral cRequiredSignatures) $ fmap RequireSignature (fundingPKeyHash : cOtherSignatories)

  (sKey, _) = fromJust $ signingKeyFromConfig config

  fundingPKeyHash ∷ GYPaymentKeyHash
  fundingPKeyHash =
    case sKey of
      AGYPaymentSigningKey skey' → paymentKeyHash . paymentVerificationKey $ skey'
      AGYExtendedPaymentSigningKey skey' → getExtendedVerificationKey skey' & extendedVerificationKeyHash

tokenAddressFromConfig ∷ Config → GYAddress
tokenAddressFromConfig config@Config {..} = addressFromSimpleScript cNetworkId $ simpleScriptFromConfig config
