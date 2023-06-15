********************************************
Contributing to the ``cardano-cli`` project
********************************************

Updating dependencies
====

... from Hackage
----

Updating package dependencies from Hackage should work like normal in a Haskell project.
The most important thing to note is that we pin the ``index-state`` of the Hackage package index in ``cabal.project``.
This means that cabal will always see Hackage “as if” it was that time, ensuring reproducibility.
But it also means that if you need a package version that was released *after* that time, you need to bump the ``index-state`` (and to run ``cabal update`` locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you will also need to pull in the Nix equivalent of the newer ``index-state``.
You can do this by running ``nix flake lock --update-input haskellNix/hackage``.

... from the Cardano package repository
----

Many Cardano packages are not on Hackage and are instead in the `Cardano package repository <https://github.com/input-output-hk/cardano-haskell-packages>`__, see the README for (lots) more information.
Getting new packages from there works much like getting them from Hackage.
The differences are that it has an independent ``index-state``, and that there is a different Nix command you need to run afterwards: ``nix flake lock --update-input CHaP``.

Using unreleased versions of dependencies
~~~~

Sometimes we need to use an unreleased version of one of our dependencies, either to fix an issue in a package that is not under our control, or to experiment with a pre-release version of one of our own packages.
You can use a ``source-repository-package`` stanza to pull in the unreleased version.
Try only to do this for a short time, as it does not play very well with tooling, and will interfere with the ability to release the node itself.

For packages that we do not control, we can end up in a situation where we have a fork that looks like it will be long-lived or permanent (e.g. the maintainer is unresponsive, or the change has been merged but not released).
In that case, release a patched version to the `Cardano package repository <https://github.com/input-output-hk/cardano-haskell-packages>`__, which allows us to remove the ``source-repository-package`` stanza.
See the README for instructions.
