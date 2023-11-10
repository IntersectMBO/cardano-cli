{
  description = "cardano-cli";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    incl.url = "github:divnix/incl";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP.url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      # this is slow as we don't have aarch64-linux native builders as of 2024-04-03
      # disabling to reduce CI time initially. Uncomment later
      # When you uncomment, lookup the "TODO generalize" comments in release-upload.yaml
      # "aarch64-linux"
      "aarch64-darwin"
    ];

    # see flake `variants` below for alternative compilers
    defaultCompiler = "ghc965";
    haddockShellCompiler = defaultCompiler;

    cabalHeadOverlay = final: prev: {
      cabal-head =
        (final.haskell-nix.cabalProject {
          # cabal master commit containing https://github.com/haskell/cabal/pull/8726
          # this fixes haddocks generation
          src = final.fetchFromGitHub {
            owner = "haskell";
            repo = "cabal";
            rev = "6eaba73ac95c62f8dc576e227b5f9c346910303c";
            hash = "sha256-Uu/w6AK61F7XPxtKe+NinuOR4tLbaT6rwxVrQghDQjo=";
          };
          index-state = "2024-07-03T00:00:00Z";
          compiler-nix-name = haddockShellCompiler;
          cabalProject = ''
            packages: Cabal-syntax Cabal cabal-install-solver cabal-install
          '';
          configureArgs = "--disable-benchmarks --disable-tests";
        })
        .cabal-install
        .components
        .exes
        .cabal;
    };
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
            cabalHeadOverlay
          ];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

        gitRevFlag =
          if inputs.self ? rev
          then [("--ghc-option=-D__GIT_REV__=\\\"" + inputs.self.rev + "\\\"")]
          else [];

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
              # for now we're using latest cabal for `cabal haddock-project` fixes
              # cabal = "3.10.3.0";
              ghcid = "0.8.8";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # tools that work only with default compiler
              haskell-language-server.src = nixpkgs.haskell-nix.sources."hls-2.6";
              hlint = "3.6.1";
              stylish-haskell = "0.14.5.0";
            };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs; [gh jq yq-go actionlint shellcheck cabal-head];
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              packages.cardano-cli.configureFlags = ["--ghc-option=-Werror"] ++ gitRevFlag;
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
            }
            // {haddockShell = devShells.haddockShell;};
          legacyPackages = rec {
            inherit cabalProject nixpkgs;
            # also provide hydraJobs through legacyPackages to allow building without system prefix:
            inherit hydraJobs;
            # expose cardano-cli binary at top-level
            cardano-cli = cabalProject.hsPkgs.cardano-cli.components.exes.cardano-cli;
          };
          devShells = let
            profilingShell = p: {
              # `nix develop .#profiling` (or `.#ghc927.profiling): a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profilingShell cabalProject
            # Add GHC 9.6 shell for haddocks
            // {
              haddockShell = let
                p = cabalProject.appendModule {compiler-nix-name = haddockShellCompiler;};
              in
                p.shell // (profilingShell p);
            };

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
