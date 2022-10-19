# Plutus Platform Animal Shelter

This is my final project for the Cardano Developer Professional program.

## Setting up

### Cabal+Nix build

Alternatively, use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

Otherwise, you can use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```
$ echo "use nix" > .envrc # Or manually add "use nix" in .envrc if you already have one
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

## Features

### Animal Reference Tokens
An animal token represents a single animal in the blockchain. This serves as the reference token (100) for user tokens (222, 333, etc.). These tokens are locked within a script and contains datum.

### Animal User Tokens


### Donation Tokens

### 

## Datum - Parameters
Wallets which are allowed to mint
Minimum donation amount
Treasure wallet

## Redeemer - Actions
-- Mint (only certain wallets can mint, reference token must be owned by the script)
-- Donate (any wallet can donate, check if reference token exists)

Treasury Address - Where the donation

Only allow minting if donation was made to the treasury

ScriptContext

What to check
- Donation was made to the treasury address
- FT was minted
- 

TxInfo
txInfoOutputs
- Check if correct NFT is minted
- Check if there's a donation to the treasury

Mint validation
- both (100) and (200) are minted

