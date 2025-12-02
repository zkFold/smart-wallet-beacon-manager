module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Data.Maybe (fromMaybe)
import Options.Applicative

import ZkFold.Cardano.SmartWallet.Beacon.Config (Config (..), configOptionalFPIO)
import ZkFold.Cardano.SmartWallet.Beacon.Manager

newtype MintCommand = MintCommand (Maybe FilePath)

newtype UpdateCommand = UpdateCommand (Maybe FilePath)

data Command = CMint MintCommand | CUpdate UpdateCommand

parseConfigPath ∷ Parser Command
parseConfigPath =
  subparser $
    mconcat
      [ command
          "mint"
          ( info (CMint <$> parseMintCommand <**> helper) $
              progDesc "Mint beacon token"
          )
      , command
          "update"
          ( info (CUpdate <$> parseUpdateCommand <**> helper) $
              progDesc "Continuously check for beacon token updates"
          )
      ]

configPathParser ∷ Parser (Maybe FilePath)
configPathParser =
  optional
    ( strOption
        ( long "config"
            <> metavar "CONFIG"
            <> short 'c'
            <> help "Path of optional configuration file. If not provided, \"CONFIG\" environment variable is used."
        )
    )

parseUpdateCommand ∷ Parser UpdateCommand
parseUpdateCommand = UpdateCommand <$> configPathParser

parseMintCommand ∷ Parser MintCommand
parseMintCommand = MintCommand <$> configPathParser

main ∷ IO ()
main = do
  execParser opts >>= runCommand
 where
  opts =
    info
      (parseConfigPath <**> helper)
      ( fullDesc
          <> progDesc "Beacon manager"
          <> header "Mint or update beacon token"
      )

runCommand ∷ Command → IO ()
runCommand (CMint (MintCommand mfp)) = do
  config ← configOptionalFPIO mfp
  void $ manageBeacon config Create
runCommand (CUpdate (UpdateCommand mfp)) = do
  config ← configOptionalFPIO mfp
  -- Do not set to more than 2000 seconds. It will be more than a 32-bit Int from threadDelay can handle
  let interval = fromMaybe 1800 (cUpdateInterval config)
  forever $ do
    void $ manageBeacon config Update
    threadDelay (fromIntegral interval * 1000000)
