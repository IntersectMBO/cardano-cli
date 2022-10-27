help: ## Print documentation
	@{ grep -hE '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST); echo -e '$(EXTRA_HELP)'; } | sed 's/^ //' | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-33s\033[0m %s\n", $$1, $$2}'

include lib.mk
include nix.mk

PROJECT_NAME = cardano-node
NUM_PROC     = $(nproc --all)

## One of:  shey alra mary alzo bage
ERA     ?= bage

PROFILE ?= default-${ERA}
REV     ?= master
ITER    ?=
ARGS    ?=
CMD     ?=
RUN     ?=

lint hlint: ## Run the CI version of hlint
	nix build --no-link '.#checks/hlint' --cores 0
haddock-hoogle haddocks hoogle:
	if test -z "$$IN_NIX_SHELL"; then nix-shell --run 'cabal haddock all --haddock-hoogle'; else cabal haddock all --haddock-hoogle; fi
host-hlint: ## Run the system (not Nix) version of hlint
	hlint bench cardano-{api,cli,client-demo,node,node-capi,node-chairman,submit-api,testnet,tracer}

stylish-haskell: ## Apply stylish-haskell on all *.hs files
	@find . -type f -name "*.hs" -not -path '.git' -print0 | xargs -0 stylish-haskell -i

cabal-hashes:
	nix run .#checkCabalProject

cli node:
	cabal --ghc-options="+RTS -qn8 -A32M -RTS" build cardano-$@

trace-documentation:
	cabal run -- exe:cardano-node trace-documentation --config 'configuration/cardano/mainnet-config-new-tracing.yaml' --output-file 'doc/new-tracing/tracers_doc_generated.md'

###
### Workbench
###
CI_TARGETS := hlint workbench-ci-test haddock-hoogle
ci:  ci-report ci-targets
ci-report:
	@echo -e "\033[34mGoals under test\033[0m:  \033[33m$(CI_TARGETS)\033[0m"
ci-targets:  $(CI_TARGETS)

##
## Base targets:
##
shell:                                           ## Nix shell, (workbench from /nix/store), vars: PROFILE, CMD, RUN
	nix-shell -A 'workbench-shell' --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE} ${ARGS} ${if ${CMD},--command "${CMD}"} ${if ${RUN},--run "${RUN}"}
shell-dev shell-prof shell-nix: shell
shell-nix: ARGS += --arg 'workbenchDevMode' false ## Nix shell, (workbench from Nix store), vars: PROFILE, CMD, RUN
shell-prof: ARGS += --arg 'profiled' true        ## Nix shell, everything Haskell built profiled

analyse: RUN := wb analyse std ${TAG}
analyse: shell

list-profiles:                                   ## List workbench profiles
	nix build .#workbench.profile-names-json --json | jq '.[0].outputs.out' -r | xargs jq .
show-profile:                                    ## NAME=profile-name
	@test -n "${NAME}" || { echo 'HELP:  to specify profile to show, add NAME=profle-name' && exit 1; }
	nix build .#all-profiles-json --json --option substitute false | jq '.[0].outputs.out' -r | xargs jq ".\"${NAME}\" | if . == null then error(\"\n###\n### Error:  unknown profile: ${NAME}  Please consult:  make list-profiles\n###\") else . end"
ps:                                              ## Plain-text list of profiles
	@nix build .#workbench.profile-names-json --json | jq '.[0].outputs.out' -r | xargs jq '.[]' --raw-output

##
## Profile-based cluster shells (autogenerated targets)
##
PROFILES_BASE         := default plutus oldtracing
PROFILES_STARTSTOP    := startstop startstop-p2p startstop-plutus startstop-notracer startstop-oldtracing
PROFILES_CI_TEST      := ci-test ci-test-p2p ci-test-plutus ci-test-notracer ci-test-dense10
PROFILES_CI_BENCH     := ci-bench ci-bench-p2p ci-bench-plutus ci-bench-notracer
PROFILES_10           := 10 10-p2p 10-plutus 10-notracer
PROFILES_FORGE_STRESS := forge-stress forge-stress-p2p forge-stress-plutus forge-stress-plutus-singleton forge-stress-notracer
PROFILES_FORGE_STRESS_PRE := forge-stress-pre forge-stress-pre-plutus forge-stress-pre-notracer
PROFILES_CHAINSYNC    := chainsync-early-byron  chainsync-early-byron-notracer  chainsync-early-byron-oldtracing
PROFILES_CHAINSYNC    += chainsync-early-alonzo chainsync-early-alonzo-notracer chainsync-early-alonzo-oldtracing chainsync-early-alonzo-p2p
PROFILES_VENDOR       := dish dish-plutus dish-10M dish-10M-plutus

SHELL_PROFILES += $(PROFILES_BASE)
SHELL_PROFILES += $(PROFILES_STARTSTOP)
SHELL_PROFILES += $(PROFILES_CI_TEST)
SHELL_PROFILES += $(PROFILES_CI_BENCH)
SHELL_PROFILES += $(PROFILES_10)
SHELL_PROFILES += $(PROFILES_FORGE_STRESS)
SHELL_PROFILES += $(PROFILES_FORGE_STRESS_PRE)
SHELL_PROFILES += $(PROFILES_CHAINSYNC)
SHELL_PROFILES += $(PROFILES_VENDOR)

## Note:  to enable a shell for a profile, just add its name (one of names from 'make ps') to SHELL_PROFILES

$(eval $(call define_profile_targets,$(SHELL_PROFILES)))

###
### Misc
###
clean-profile proclean:
	rm -f *.html *.prof *.hp *.stats *.eventlog

clean: clean-profile
	rm -rf logs/ socket/ cluster.*

full-clean: clean
	rm -rf db dist-newstyle $(shell find . -name '*~' -or -name '*.swp')

cls:
	echo -en "\ec"

.PHONY: cabal-hashes clean cli cls cluster-profiles help node run-test shell shell-dev stylish-haskell $(SHELL_PROFILES) workbench-ci-test