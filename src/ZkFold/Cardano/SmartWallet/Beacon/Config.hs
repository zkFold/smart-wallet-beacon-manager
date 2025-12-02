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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Word (Word32)
import Data.Yaml qualified as Yaml
import Deriving.Aeson
import GeniusYield.GYConfig (GYCoreConfig (..), GYCoreProviderInfo)
import GeniusYield.Imports (throwIO, (&))
import GeniusYield.Types
import System.Envy
import System.FilePath (takeExtension)

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
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "c", LowerFirst]] Config

instance FromEnv Config where
  fromEnv _ = forceFromJsonOrYaml <$> env "BEACON_CONFIG"
   where
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

configOptionalFPIO ∷ Maybe FilePath → IO Config
configOptionalFPIO mfp = do
  e ← maybe decodeEnv eitherDecodeFileStrictJsonOrYaml mfp
  either (throwIO . userError) return e

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

  Just (sKey, _) = signingKeyFromConfig config

  fundingPKeyHash ∷ GYPaymentKeyHash
  fundingPKeyHash =
    case sKey of
      AGYPaymentSigningKey skey' → paymentKeyHash . paymentVerificationKey $ skey'
      AGYExtendedPaymentSigningKey skey' → getExtendedVerificationKey skey' & extendedVerificationKeyHash

tokenAddressFromConfig ∷ Config → GYAddress
tokenAddressFromConfig config@Config {..} = addressFromSimpleScript cNetworkId $ simpleScriptFromConfig config
