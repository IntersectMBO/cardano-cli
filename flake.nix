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
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      # disabling to reduce CI time initially. Uncomment later
      #"x86_64-darwin"
      #"aarch64-linux"
      #"aarch64-darwin"
    ];
  in
    {inherit (inputs) incl;}
    // inputs.flake-utils.lib.eachSystem supportedSystems (
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

        gitRevFlag = if inputs.self ? rev
                     then [ ("--ghc-option=-D__GIT_REV__=\\\"" + inputs.self.rev + "\\\"") ]
                     else [];

        # see flake `variants` below for alternative compilers
        defaultCompiler = "ghc964";
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
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };
          # tools we want in our shell, from hackage
          shell.tools =
            {
              cabal = "3.10.2.0";
              ghcid = "0.8.8";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # tools that work only with default compiler
              haskell-language-server.src = nixpkgs.haskell-nix.sources."hls-2.6";
              hlint = "3.6.1";
              stylish-haskell = "0.14.5.0";
            };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs; [ gh jq yq-go ];
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              packages.cardano-cli.configureFlags = [ "--ghc-option=-Werror" ] ++ gitRevFlag;
              packages.cardano-cli.components.tests.cardano-cli-test.build-tools =
                with pkgs.buildPackages; [ jq coreutils shellcheck ];
              packages.cardano-cli.components.tests.cardano-cli-golden.build-tools =
                with pkgs.buildPackages; [ jq coreutils shellcheck ];
            })
            ({
              pkgs,
              config,
              ...
            }: let
              exportCliPath = "export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}";
              mainnetConfigFiles = [
                "configuration/cardano/mainnet-config.yaml"
                "configuration/cardano/mainnet-config.json"
                "configuration/cardano/mainnet-byron-genesis.json"
                "configuration/cardano/mainnet-shelley-genesis.json"
                "configuration/cardano/mainnet-alonzo-genesis.json"
                "configuration/cardano/mainnet-conway-genesis.json"
              ];
            in {
              # cardano-cli tests depend on cardano-cli and some config files:
              packages.cardano-cli.components.tests.cardano-cli-golden.preCheck = let
                # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                filteredProjectBase = inputs.incl ./. [
                  "cabal.project"
                  "scripts/plutus/scripts/v1/custom-guess-42-datum-42.plutus"
                ];
              in ''
                ${exportCliPath}
                cp -r ${filteredProjectBase}/* ..
              '';
              packages.cardano-cli.components.tests.cardano-cli-test.preCheck = let
                # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                filteredProjectBase = inputs.incl ./. mainnetConfigFiles;
              in ''
                ${exportCliPath}
                cp -r ${filteredProjectBase}/* ..
              '';
            })
            {
               packages.crypton-x509-system.postPatch = ''
                  substituteInPlace crypton-x509-system.cabal --replace 'Crypt32' 'crypt32'
               '';
            }
          ];
        });
        # ... and construct a flake from the cabal project
        flake = cabalProject.flake (
          lib.optionalAttrs (system == "x86_64-linux") {
            # on linux, build/test other supported compilers
            variants = lib.genAttrs ["ghc8107"] (compiler-nix-name: {
              inherit compiler-nix-name;
            });
          }
        );
      in
        lib.recursiveUpdate flake rec {
          project = cabalProject;
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
            # expose cardano-cli binary at top-level
            cardano-cli = cabalProject.hsPkgs.cardano-cli.components.exes.cardano-cli;
          };
          devShells = let
            profillingShell = p: {
              # `nix develop .#profiling` (or `.#ghc927.profiling): a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profillingShell cabalProject;
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
