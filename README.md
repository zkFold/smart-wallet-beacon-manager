# zkFold Smart Wallet Beacon manager 

This repository houses Beacon token manager to mint or update the token which stores Zero-Knowledge setup for the ZK based smart wallet by [zkFold](https://zkfold.io/). 

## Building locally from source using the Haskell Toolchain

1. Prepare a configuration, which can be stored either in file or in `BEACON_CONFIG` environment variable. Structure of it is as follows:

    ```yaml
     # Path to file contains maestro token which is used to create blockchain provider used by Atlas, our off-chain transaction building tool. Also can be stored in `MAESTRO_API_KEY` environment variable. Now we only support the Maestro provider
    maestroApiKeyPath: ./secrets/maestro-token
    turboSubmit: false
 
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
    fundMnemonicPath: ./secrets/beacon-mnemonic.yaml
     
     # Account index.
    fundAccIx: 0
     # Payment address index.
    fundAddrIx: 0

    otherSignatories: []

    requiredSignatures: 1

     # Beacon token name and minting policy
    tokenName: ""
    policyId: "77c6b433239c2e79f20a017d6e08d59bf9a1bce3fa3348d170ced87b"

     # Beacon update interval in seconds
    updateInterval: 120

    tokensToMint: 1
    ```

    Here you can find examples of secret parameters:
    
    `maestro-token`: 
    ```
    YOUR_MAESTRO_TOKEN
    ```

    `beacon-mnemonic.yaml`:
    ```yaml
    - funding
    - wallets
    - mnemonic
    ```

2. Run the manager with command `cabal run -- beacon-manager update -c my-config.yaml`.

   Call: `cabal run beacon-manager -- -h` for help. 😉
