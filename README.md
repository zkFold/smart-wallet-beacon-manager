# zkFold Smart Wallet Beacon manager 

This repository houses Beacon token manager to mint or update the token which stores Zero-Knowledge setup for the ZK based smart wallet by [zkFold](https://zkfold.io/). 

## Building locally from source using the Haskell Toolchain

1. Prepare a configuration, which can be stored either in file or in `BEACON_CONFIG` environment variable. Structure of it is as follows:

    ```yaml
     # Blockchain provider used by Atlas, our off-chain transaction building tool.
     # Head over to https://atlas-app.io/getting-started/endpoints#providing-data-provider section to know how to configure `coreProvider` and what all options are available for it.
    coreProvider:
      maestroToken: YOUR_MAESTRO_TOKEN 
      turboSubmit: false
    
    tokenName: "7a6b466f6c64" #zkFold in hex
    policyId: "f34289c768c672ba37dec7c8f84392ee316e8392eaaa0ec45907eb15" 
    
     # The number of tokens to mint
    tokensToMint: 1
    
    updateInterval: 60
    
     # Network id, only `mainnet` and `preprod` are supported for at the moment.
    networkId: preprod 
     # Logging configuration. It's an array to cater for potentially multiple scribes.
     # See it's description mentioned at https://atlas-app.io/getting-started/endpoints#providing-data-provider for more information.
    logging:
      - type:
          tag: stderr
         # Possible values of `severity` are `Debug`, `Info`, `Warning` and `Error`.
        severity: Debug
         # Possible values of `verbosity` are `V0`, `V1`, `V2`, `V3` and `V4`. Consult https://hackage.haskell.org/package/katip-0.8.8.0/docs/Katip.html#t:Verbosity for more information about it.
        verbosity: V2
    
    otherSignatories: []
    
    requiredSignatures: 1
    
    fundMnemonic:
      - mnemonic
      - to
      - fund
      - beacon
      - updates
    # Account index.
    fundAccIx: 0
    # Payment address index.
    fundAddrIx: 0
    ```
2. Run the manager with command `cabal run -- beacon-manager update -c my-config.yaml`.

   Call: `cabal run beacon-manager -- -h` for help. 😉
