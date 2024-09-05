# Contributing to the `cardano-cli` project

The `cardano-cli` development is primarily with Haskell tools.

These can be provisioned with:

```bash
nix develop github:input-output-hk/devx#ghc8107-minimal-iog
```

## Roles and Responsibilities
We maintain a [CODEOWNERS file](CODEOWNERS) which provides information who
should review a contributing PR.  Note that you might need to get approvals
from all code owners (even though GitHub doesn't give a way to enforce it).

## Updating dependencies

### From Hackage
Updating package dependencies from Hackage should work like normal in a Haskell project.
The most important thing to note is that we pin the `index-state` of the Hackage package index in `cabal.project`.
This means that cabal will always see Hackage “as if” it was that time, ensuring reproducibility.
But it also means that if you need a package version that was released *after* that time, you need to bump the `index-state` (and to run `cabal update` locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you will also need to pull in the Nix equivalent of the newer `index-state`.
You can do this by running `nix flake lock --update-input haskellNix/hackage`.

### from the Cardano package repository
Many Cardano packages are not on Hackage and are instead in the [Cardano Haskell Package (CHaP) repository][CHaP].

Getting new packages from there works much like getting them from Hackage.

The differences are that it has an independent `index-state`, and that there is a different Nix command you need to run afterwards: `nix flake lock --update-input CHaP`.

## Using unreleased versions of dependencies
Sometimes we need to use an unreleased version of one of our dependencies, either to fix an issue in a package that is not under our control, or to experiment with a pre-release version of one of our own packages.
You can use a `source-repository-package` stanza to pull in the unreleased version.

Try only to do this for a short time, as it does not play very well with tooling, and will interfere with the ability to release the `cardano-cli` itself. If you do add a temporary `source-repository-package` stanza, you need to
provide a [`--sha256` comment in `cabal.project`](https://input-output-hk.github.io/haskell.nix/tutorials/source-repository-hashes.html#cabalproject).

For packages that we do not control, we can end up in a situation where we have a fork that looks like it will be long-lived or permanent (e.g. the maintainer is unresponsive, or the change has been merged but not released).
In that case, release a patched version to the [CHaP repository][CHaP], which allows us to remove the `source-repository-package` stanza.

## Troubleshooting

### `<stdout>: commitBuffer: invalid argument (invalid character)` error when launching tests
If pretty much any test (launched with `cabal test`) fails with
```
<stdout>: commitBuffer: invalid argument (invalid character)
```
or
```
withFile: invalid argument (invalid byte sequence)
```
1. Make sure that you have file `/usr/lib/locale/locale-archive` present in your system.
    * If you don't have `locale-archive` file present on your system, you need to generate it.
      Please consult your system distribution manual how to configure locale and generate that file.
1. Execute `export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive` in your shell (consider doing this automatically when entering the repository, with [direnv](https://direnv.net/)).

## Releasing a version of the `cardano-cli` to the Cardano Haskell Packages repository

When releasing a new version of `cardano-cli`, it and the other packages in this repository should be released to the [CHaP repository][CHaP].

See the [RELEASING.md](RELEASING.md) for instructions.

Please note that libraries need bounds on the version of their dependencies to avoid bitrot and be effectively reusable.

[CHaP]: https://github.com/input-output-hk/cardano-haskell-packages

## Improving `git blame`

You can have `git blame` ignore the reformatting done when [introducting fourmolu](https://github.com/IntersectMBO/cardano-cli/pull/835/commits/9d1fd093071278be77428dba189d958d5b7a7aeb) by specifying `blame.ignoreRevsFile` to `.git-blame-ignore-revs` in your repository's git configuration. Do it as follows:

```shell
git config --local blame.ignoreRevsFile .git-blame-ignore-revs
```
