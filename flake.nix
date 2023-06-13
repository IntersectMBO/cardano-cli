{
  description = "cardano-cli";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    incl.url = "github:divnix/incl";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror";
    cardano-mainnet-mirror.flake = false;
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];
  in { inherit (inputs) incl; } //
  inputs.flake-utils.lib.eachSystem supportedSystems (
    system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = [
            # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
            inputs.iohkNix.overlays.crypto
            # haskellNix.overlay can be configured by later overlays, so need to come before them.
            inputs.haskellNix.overlay
            # configure haskell.nix to use iohk-nix crypto librairies.
            inputs.iohkNix.overlays.haskell-nix-crypto
          ];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

        # see flake `variants` below for alternative compilers
        defaultCompiler = "ghc927";
        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' ({config, ...}: {
          src = ./.;
          name = "cardano-cli";
          compiler-nix-name = lib.mkDefault defaultCompiler;

          # we also want cross compilation to windows on linux (and only with default compiler).
          crossPlatforms = p:
          lib.optional (system == "x86_64-linux" && config.compiler-nix-name == defaultCompiler)
          p.mingwW64;

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
          };
          # tools we want in our shell, from hackage
          shell.tools =
            {
              cabal = "3.10.1.0";
              ghcid = "0.8.8";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # tools that work only with default compiler
              fourmolu = "0.10.1.0";
              hlint = "3.5";
              #haskell-language-server = "2.0.0.0";
            };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs; [
            haskellPackages.implicit-hie
          ];
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              packages.byron-spec-chain.configureFlags = ["--ghc-option=-Werror"];
              packages.byron-spec-ledger.configureFlags = ["--ghc-option=-Werror"];
              packages.delegation.configureFlags = ["--ghc-option=-Werror"];
              packages.non-integral.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-cli-shelley.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-cli-shelley-ma.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-cli-shelley-ma-test.configureFlags = ["--ghc-option=-Werror"];
              packages.small-steps.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-cli-byron = {
                configureFlags = ["--ghc-option=-Werror"];
                components = {
                  tests.cardano-cli-byron-test = {
                    preCheck = ''
                      export CARDANO_MAINNET_MIRROR="${inputs.cardano-mainnet-mirror}/epochs"
                      cp ${./eras/byron/ledger/impl/mainnet-genesis.json} ./mainnet-genesis.json
                    '';
                    testFlags = ["--scenario=ContinuousIntegration"];
                  };
                };
              };
              packages.cardano-cli.components.tests.cardano-cli-test.build-tools =
                lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck ]);
              packages.cardano-cli.components.tests.cardano-cli-golden.build-tools =
                lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck ]);
              })
            ({ pkgs, config, ... }:
             let
               exportCliPath = "export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}";
               mainnetConfigFiles = [
                 "configuration/cardano/mainnet-config.yaml"
                 "configuration/cardano/mainnet-config.json"
                 "configuration/cardano/mainnet-byron-genesis.json"
                 "configuration/cardano/mainnet-shelley-genesis.json"
                 "configuration/cardano/mainnet-alonzo-genesis.json"
                 "configuration/cardano/mainnet-conway-genesis.json"
               ];
               goldenConfigFiles = [
                 "cardano-cli/test/cardano-cli-golden/files/golden/alonzo/genesis.alonzo.spec.json"
                 "cardano-cli/test/cardano-cli-golden/files/golden/conway/genesis.conway.spec.json"
               ];
             in
             {
               # cardano-cli tests depend on cardano-cli and some config files:
               packages.cardano-cli.components.tests.cardano-cli-golden.preCheck =
                 let
                   # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                   filteredProjectBase = inputs.incl ./. [
                     "cabal.project"
                     "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"
                   ];
                 in
                 ''
                   ${exportCliPath}
                   cp -r ${filteredProjectBase}/* ..
                 '';
               packages.cardano-cli.components.tests.cardano-cli-test.preCheck =
                 let
                   # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                   filteredProjectBase = inputs.incl ./. mainnetConfigFiles;
                 in
                 ''
                   ${exportCliPath}
                   cp -r ${filteredProjectBase}/* ..
                 '';
            })
            ({pkgs, ...}:
            lib.mkIf pkgs.stdenv.hostPlatform.isUnix {
              packages.cardano-cli-shelley-ma-test.components.tests.cardano-ledger-shelley-ma-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
              packages.cardano-cli-shelley-test.components.tests.cardano-ledger-shelley-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
              packages.cardano-cli-alonzo-test.components.tests.cardano-ledger-alonzo-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
              packages.cardano-cli-babbage-test.components.tests.cardano-ledger-babbage-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
              packages.cardano-cli-conway-test.components.tests.cardano-ledger-conway-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
            })
            ({pkgs, ...}:
            lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
              packages.set-algebra.components.tests.tests.buildable = lib.mkForce false;
              packages.plutus-preprocessor.package.buildable = lib.mkForce false;
              packages.cardano-cli-test.package.buildable = lib.mkForce false;
              packages.cardano-cli-shelley-ma-test.package.buildable = lib.mkForce false;
              packages.cardano-cli-shelley-test.package.buildable = lib.mkForce false;
              packages.cardano-cli-alonzo-test.package.buildable = lib.mkForce false;
              packages.cardano-cli-babbage-test.package.buildable = lib.mkForce false;
              packages.cardano-cli-conway-test.package.buildable = lib.mkForce false;
            })
          ];
        });
        # ... and construct a flake from the cabal project
        flake =
          cabalProject.flake (
            lib.optionalAttrs (system == "x86_64-linux") {
              # on linux, build/test other supported compilers
              variants = lib.genAttrs ["ghc8107"] (compiler-nix-name: {
                inherit compiler-nix-name;
              });
            }
            );
      in
      lib.recursiveUpdate flake rec {
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
                };
              };
              legacyPackages = rec {
                inherit cabalProject nixpkgs;
            # also provide hydraJobs through legacyPackages to allow building without system prefix:
            inherit hydraJobs;
          };
          devShells = let
            profillingShell = p: {
              # `nix develop .#profiling` (or `.#ghc927.profiling): a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
          profillingShell cabalProject
            # Additional shells for every GHC version supported by haskell.nix, eg. `nix develop .#ghc927`
            // lib.mapAttrs (compiler-nix-name: _: let
              p = cabalProject.appendModule {inherit compiler-nix-name;};
            in
            p.shell // (profillingShell p))
            nixpkgs.haskell-nix.compiler;
          # formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
        );

        nixConfig = {
          extra-substituters = [
            "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
