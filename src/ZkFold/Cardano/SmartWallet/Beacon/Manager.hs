module ZkFold.Cardano.SmartWallet.Beacon.Manager (
  manageBeacon,
  BeaconOp (..),
) where

import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic)
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.GYConfig (GYCoreConfig (..), withCfgProviders)
import GeniusYield.TxBuilder (
  buildTxBody,
  gyLogDebug',
  gyLogInfo',
  mustHaveInput,
  mustHaveOutput,
  runGYTxMonadIO,
  runGYTxQueryMonadIO,
  signAndSubmitConfirmed,
 )
import GeniusYield.TxBuilder.IO (liftBuilderMonad)
import GeniusYield.TxBuilder.IO.Unsafe (unsafeIOToTxBuilderMonad)
import GeniusYield.Types

import ZkFold.Cardano.SmartWallet.Beacon.Config
import ZkFold.Cardano.SmartWallet.Beacon.Types
import ZkFold.Cardano.SmartWallet.Beacon.Utils

data BeaconOp = Create | Update
  deriving stock (Generic, Show)

manageBeacon ∷ Config → BeaconOp → IO (GYTxOutRef, [JwtVerificationKey])
manageBeacon config@Config {..} op = do
  currentKeys ← getCurrentKeys Google

  oref ← withCfgProviders coreConfig "manage-beacon-token" $ \providers → do
    runGYTxQueryMonadIO nid providers $
      gyLogDebug' "" $
        "current keys: " <> T.unpack (T.intercalate " - " $ fmap jvkKid currentKeys)
    runGYTxMonadIO nid providers signingKey Nothing [signingKeyAddress] signingKeyAddress Nothing $ do
      case op of
        Create → do
          gyLogInfo' "" $
            "Minting "
              ++ show cTokensToMint
              ++ " "
              ++ show cTokenName
              ++ " to "
              ++ show tokenMultisigAddress
              ++ " using wallet "
              ++ show signingKeyAddress
          (mintedToken, mintSkel) ← mintTestTokens cTokenName cTokensToMint
          gyLogInfo' "" $ "minted token assetclass: " <> show mintedToken

          gyLogInfo' "" $ "Started computing datum"
          setupMap ← liftBuilderMonad . unsafeIOToTxBuilderMonad $ mapSetupBytes currentKeys
          let datum = makeDatum setupMap
          gyLogInfo' "" $ "Finished computing datum"

          mintBody ←
            buildTxBody $
              mintSkel
                <> mustHaveOutput
                  (makeDatumInline $ mkGYTxOut tokenMultisigAddress (valueSingleton mintedToken (fromIntegral cTokensToMint)) datum)
          tid ← signAndSubmitConfirmed mintBody
          gyLogInfo' "" $ "minted token txid: " <> show tid
          pure (txOutRefFromTuple (tid, 0))
        Update → do
          gyLogInfo' "" "Locating beacon token"
          (txIn, _, keysPrev) ← findBeaconUtxo beaconToken
          if S.fromList keysPrev /= S.fromList (fmap jvkKid currentKeys)
            then do
              gyLogInfo' "" $ "Updating datum for token " ++ show cTokenName

              gyLogInfo' "" $ "Started computing datum"
              setupMap ← liftBuilderMonad . unsafeIOToTxBuilderMonad $ mapSetupBytes currentKeys
              let datum = makeDatum setupMap
              gyLogInfo' "" $ "Finished computing datum"

              updateBody ←
                buildTxBody $
                  mustHaveInput (GYTxIn txIn $ GYTxInWitnessSimpleScript $ GYBuildSimpleScriptInlined simpleScript)
                    <> mustHaveOutput
                      ( makeDatumInline $
                          mkGYTxOut tokenMultisigAddress (valueSingleton (GYToken cPolicyId cTokenName) (fromIntegral cTokensToMint)) datum
                      )
              tid ← signAndSubmitConfirmed updateBody
              gyLogInfo' "" $ "Datum update txid: " <> show tid
              pure (txOutRefFromTuple (tid, 0))
            else do
              gyLogInfo' "" "No update needed for token"
              pure txIn
  pure (oref, currentKeys)
 where
  (signingKey, signingKeyAddress) = 
      case signingKeyFromConfig config of
        Just ka -> ka
        _       -> error "Could not obtail signing key from config"

  tokenMultisigAddress = tokenAddressFromConfig config

  simpleScript = simpleScriptFromConfig config

  beaconToken = BeaconToken cPolicyId cTokenName (addressToBech32 tokenMultisigAddress)

  coreConfig = coreConfigFromConfig config

  nid = cfgNetworkId coreConfig
