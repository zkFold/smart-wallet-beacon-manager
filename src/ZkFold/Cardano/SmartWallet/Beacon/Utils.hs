module ZkFold.Cardano.SmartWallet.Beacon.Utils (makeDatumInline) where

import GeniusYield.Types

makeDatumInline ∷ GYTxOut PlutusV2 → GYTxOut PlutusV2
makeDatumInline txOut = txOut {gyTxOutDatum = fmap (const GYTxOutUseInlineDatum) <$> gyTxOutDatum txOut}
