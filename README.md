# zkFold Smart Wallet Beacon manager 

This repository houses Beacon token manager to mint or update the token which stores Zero-Knowledge setup for the ZK based smart wallet by [zkFold](https://zkfold.io/). 

## Building locally from source using the Haskell Toolchain

1. Prepare a configuration, which can be stored either in file or in `BEACON_CONFIG` environment variable. Structure of it is as follows:

    ```yaml
     # Path to file (.json or .yaml) contains blockchain provider used by Atlas, our off-chain transaction building tool. Also can be stored in `CORE_PROVIDER` environment variable.
     # Head over to https://atlas-app.io/getting-started/endpoints#providing-data-provider section to know how to configure `coreProvider` and what all options are available for it.
    coreProviderPath: PATH_TO_YOUR_MAESTRO_TOKEN


     # Network id, only `mainnet` and `preprod` are supported for at the moment.
    networkId: mainnet
     # Logging configuration. It's an array to cater for potentially multiple scribes.
     # See it's description mentioned at https://atlas-app.io/getting-started/endpoints#providing-data-provider for more information.
    logging:
      - type:
          tag: stderr
         # Possible values of `severity` are `Debug`, `Info`, `Warning` and `Error`.
        severity: Debug
         # Possible values of `verbosity` are `V0`, `V1`, `V2`, `V3` and `V4`. Consult https://hackage.haskell.org/package/katip-0.8.8.0/docs/Katip.html#t:Verbosity for more information about it.
        verbosity: V2

     # Wallet that provides UTxO to fund the update transaction 
    fundMnemonic:
      - funding
      - wallets
      - mnemonic

     # Account index.
    fundAccIx: 0
     # Payment address index.
    fundAddrIx: 0

    otherSignatories: []

    requiredSignatures: 0

     # Beacon token name and minting policy
    tokenName: "7a6b466f6c64"
    policyId: "982beb80d155358fad5c3b0015c4b13f7d7341835246af037009d73a"

     # Beacon update interval in seconds
    updateInterval: 1800

    tokensToMint: 0
    ```
2. Run the manager with command `cabal run -- beacon-manager update -c my-config.yaml`.

   Call: `cabal run beacon-manager -- -h` for help. 😉
