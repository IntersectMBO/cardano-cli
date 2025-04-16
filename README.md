
## Overview of the `cardano-cli` repository
`cardano-cli` refers to the command-line interface (CLI) tool for interacting with the Cardano blockchain.
Cardano is a blockchain platform that aims to provide a secure and scalable infrastructure for the development
of decentralized applications (dApps) and the execution of smart contracts.

The **`cardano-cli`** tool provides a comprehensive command-line interface for interacting with the Cardano blockchain. Its capabilities include:

- Creating and managing payment and stake keys
- Sending and receiving ada
- Managing and operating stake pools
- Registering and operating DReps (delegated representatives)
- Delegating stake (to a stake pool)
- Delegating voting power (to a DRep)
- Querying blockchain data
- Interacting with smart contracts and simple scripts
- Creating and voting on governance actions as a DRep, Stake Pool Operator (SPO), or Constitutional Committee (CC) member

## Executables

You can download the binaries of `cardano-cli` compatible with specific versions of `cardano-node` from [cardano-node's release notes](https://github.com/IntersectMBO/cardano-node/releases).

You can also download binaries of all versions of `cardano-cli` from [cardano-cli's release notes](https://github.com/IntersectMBO/cardano-cli/releases)

## Documentation

* [Tutorials](https://developers.cardano.org/docs/get-started/cardano-cli/get-started/)

Up to date command line help reference is available here:
* [List of all commands](cardano-cli/test/cardano-cli-golden/files/golden/help.cli)
* [Description of each command's options](cardano-cli/test/cardano-cli-golden/files/golden/help)

Development documentation can be found in [Cardano Node Wiki](https://github.com/input-output-hk/cardano-node-wiki/wiki).

Haddock documentation is available at: https://cardano-cli.cardano.intersectmbo.org/

## Contributing

See the [Contributing guide](CONTRIBUTING.md) for how to contribute to this project.

## Core maintainers

* [Jordan Millar](https://github.com/Jimbo4350)
* [John Ky](https://github.com/newhoggy)
* [Mateusz Gałażyn](https://github.com/carbolymer)
* [Pablo Lamela](https://github.com/palas)


[![x86\_64-linux](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-cardano-cli/master/x86_64-linux.required/shield&style=flat-square&label=x86_64-linux)](https://ci.iog.io/job/IntersectMBO-cardano-cli/master/x86_64-linux.required)
[![x86\_64-darwin](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-cardano-cli/master/x86_64-darwin.required/shield&style=flat-square&label=x86_64-darwin)](https://ci.iog.io/job/IntersectMBO-cardano-cli/master/x86_64-darwin.required)
[![aarch64-darwin](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-cardano-cli/master/aarch64-darwin.required/shield&style=flat-square&label=aarch64-darwin)](https://ci.iog.io/job/IntersectMBO-cardano-cli/master/aarch64-darwin.required)
[![GHA Build](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-cli/haskell.yml?branch=master&label=GHA%20Build&style=flat-square)](https://github.com/IntersectMBO/cardano-cli/actions/workflows/haskell.yml?query=branch%3Amaster)
[![Haddock](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-cli/github-page.yml?branch=master&label=Haddocks&style=flat-square)](https://github.com/IntersectMBO/cardano-cli/actions/workflows/github-page.yml?query=branch%3Amaster)
[![Release Upload](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-cli/release-upload.yml?label=GH%20Release%20Upload&style=flat-square)](https://github.com/IntersectMBO/cardano-cli/actions/workflows/release-upload.yml)

