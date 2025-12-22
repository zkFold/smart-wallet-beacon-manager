module ZkFold.Cardano.SmartWallet.Beacon.Types (
  BeaconToken (..),
  JwtVerificationKey (..),
  JwtVerificationKeys (..),
  JwtProvider (..),
  NaturalFromBase64Url,
  natToB64,
  b64ToNat,
  getCurrentKeys,
  mapSetupBytes,
  makeDatum,
  toGyTxOutRef,
  findBeaconUtxo,
) where

import Control.DeepSeq (NFData)
import Control.Exception (evaluate)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Casing
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL as B64
import Data.List (foldl')
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import GeniusYield.Imports (coerce, (>>>))
import GeniusYield.TxBuilder
import GeniusYield.Types
import Network.HTTP.Req
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusMap
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.OnChain.BLS12_381.F (F (..))
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes (..))
import ZkFold.Protocol.NonInteractiveProof (powersOfTauSubset)
import ZkFold.Symbolic.Examples.SmartWallet (ZKSetupBytes (..), expModCircuit, expModSetup, mkSetup)

data BeaconToken
  = BeaconToken
  { policyId ∷ !GYMintingPolicyId
  , tokenName ∷ !GYTokenName
  , address ∷ !GYAddressBech32
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype NaturalFromBase64Url = NaturalFromBase64Url Natural
  deriving newtype (Eq, NFData, Ord, Show)

instance FromJSON NaturalFromBase64Url where
  parseJSON = Aeson.withText "NaturalFromBase64Url" $ \t →
    either (fail . show) (pure . NaturalFromBase64Url . bsToNat) $ B64.decodeUnpadded (T.encodeUtf8 t)

natToB64 ∷ Natural → NaturalFromBase64Url
natToB64 = NaturalFromBase64Url

b64ToNat ∷ NaturalFromBase64Url → Natural
b64ToNat (NaturalFromBase64Url n) = n

bsToNat ∷ ByteString → Natural
bsToNat = foldl' (\a w → fromIntegral w + 256 * a) 0 . BS.unpack

data JwtVerificationKey
  = JwtVerificationKey
  { jvkUse ∷ !Text
  , jvkAlg ∷ !Text
  , jvkKty ∷ !Text
  , jvkKid ∷ !Text
  , jvkN ∷ !NaturalFromBase64Url
  , jvkE ∷ !NaturalFromBase64Url
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass NFData

instance FromJSON JwtVerificationKey where
  parseJSON = Aeson.genericParseJSON $ aesonPrefix snakeCase

newtype JwtVerificationKeys
  = JwtVerificationKeys {jvkKeys ∷ [JwtVerificationKey]}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass NFData

instance FromJSON JwtVerificationKeys where
  parseJSON = Aeson.genericParseJSON $ aesonPrefix snakeCase

data JwtProvider = Google | Apple
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass NFData

getCurrentKeys ∷ JwtProvider → IO [JwtVerificationKey]
getCurrentKeys provider = runReq defaultHttpConfig $ do
  bs ← req GET url NoReqBody bsResponse mempty
  pure $ case Aeson.eitherDecode' (BS.fromStrict $ responseBody bs) of
    Right k → jvkKeys k
    Left e → error e
 where
  url = case provider of
    Google → https "www.googleapis.com" /: "oauth2" /: "v3" /: "certs"
    Apple → https "www.appleid.apple.com" /: "auth" /: "keys"

mapSetupBytes ∷ [JwtVerificationKey] → IO [(PlutusTx.BuiltinByteString, SetupBytes)]
mapSetupBytes = mapM toKeyValue
 where
  toKeyValue ∷ JwtVerificationKey → IO (PlutusTx.BuiltinByteString, SetupBytes)
  toKeyValue jvk@JwtVerificationKey {..} = do
    setupBytes ← toSetupBytes jvk
    pure (PlutusTx.toBuiltin >>> PlutusTx.encodeUtf8 $ jvkKid, setupBytes)

  toSetupBytes ∷ JwtVerificationKey → IO SetupBytes
  toSetupBytes JwtVerificationKey {..} = do
    ts ← powersOfTauSubset
    evaluate
      . setupToPlutus
      . mkSetup
      . expModSetup @ByteString ts
      $ expModCircuit (coerce jvkE) (coerce jvkN)

makeDatum ∷ [(PlutusTx.BuiltinByteString, SetupBytes)] → GYDatum
makeDatum = datumFromPlutusData . PlutusMap.unsafeFromList

toGyTxOutRef ∷ Text → Natural → GYTxOutRef
toGyTxOutRef txHashBytes txOutput = txE
 where
  txE =
    txOutRefFromTuple
      ( fromString $ T.unpack txHashBytes
      , fromIntegral txOutput
      )

-- | Determine the latest beacon mint/update transaction,
-- retrieve the datum associated with it,
-- and decode OAuth Key IDs stored in it.
findBeaconUtxo ∷ GYTxQueryMonad m ⇒ BeaconToken → m (GYTxOutRef, [Text])
findBeaconUtxo BeaconToken {..} = do
  let beaconAssetClass = GYToken policyId tokenName
  tokenUTxOs ∷ [GYUTxO] ← utxosToList <$> utxosAtAddress (addressFromBech32 address) (Just beaconAssetClass)
  let tokenUTxOsFiltered = filter (\(utxoValue → v) → valueAssetClass v beaconAssetClass == 1) tokenUTxOs
  beaconUtxo ← case tokenUTxOsFiltered of
    [t] → pure t
    [] → error $ "Token " <> show policyId <> "." <> show tokenName <> " not found at address " <> show address
    _ → error $ "Multiple tokens " <> show policyId <> "." <> show tokenName <> " found at address " <> show address
  let txOutRef = utxoRef beaconUtxo
  keys ← case utxoOutDatum beaconUtxo of
    GYOutDatumInline datum' → do
      let datum = datumToPlutus' datum'
      let setupBytesMap ∷ PlutusMap.Map PlutusTx.BuiltinByteString SetupBytes = PlutusTx.unsafeFromBuiltinData datum
      let keys' = T.decodeUtf8 . PlutusTx.fromBuiltin <$> PlutusMap.keys setupBytesMap
      pure keys'
    _ → pure []

  pure (txOutRef, keys)

setupToPlutus ∷ ZKSetupBytes → SetupBytes
setupToPlutus ZKSetupBytes {..} =
  SetupBytes
    { n = n
    , nPrv = nPrv
    , pow = pow
    , omega = coerce omega_int
    , omegaNPrv = coerce omegaNPrv_int
    , k1 = coerce k1_int
    , k2 = coerce k2_int
    , h1_bytes = PlutusTx.toBuiltin h1_bytes
    , cmQm_bytes = PlutusTx.toBuiltin cmQm_bytes
    , cmQl_bytes = PlutusTx.toBuiltin cmQl_bytes
    , cmQr_bytes = PlutusTx.toBuiltin cmQr_bytes
    , cmQo_bytes = PlutusTx.toBuiltin cmQo_bytes
    , cmQc_bytes = PlutusTx.toBuiltin cmQc_bytes
    , cmQk_bytes = PlutusTx.toBuiltin cmQk_bytes
    , cmS1_bytes = PlutusTx.toBuiltin cmS1_bytes
    , cmS2_bytes = PlutusTx.toBuiltin cmS2_bytes
    , cmS3_bytes = PlutusTx.toBuiltin cmS3_bytes
    , cmT1_bytes = PlutusTx.toBuiltin cmT1_bytes
    , cmT2_bytes = PlutusTx.toBuiltin cmT2_bytes
    , cmT3_bytes = PlutusTx.toBuiltin cmT3_bytes
    }
