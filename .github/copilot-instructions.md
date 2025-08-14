# Copilot Instructions for cardano-cli

## Architecture Overview

This is the official CLI for interacting with the Cardano blockchain. The codebase follows a modular, era-aware architecture designed to handle Cardano's protocol evolution.

### Key Architectural Patterns

- **Era-Based Commands**: Commands are organized by Cardano eras (Byron, Shelley, Alonzo, Babbage, Conway). See `src/Cardano/CLI/EraBased/` for the main pattern.
- **Command-Run Separation**: Every command module has a corresponding run module (e.g., `Command.hs` defines parsers, `Run.hs` executes them).
- **Type-Safe Era Handling**: Uses GADTs and type families to ensure era-specific operations are type-safe. See `Cardano.CLI.EraBased.Command.AnyEraCommand`.

### Module Organization

```
src/Cardano/CLI/
├── Byron/          # Legacy Byron era commands
├── EraBased/       # Era-specific commands (main pattern)
│   ├── Genesis/    # Genesis creation and management
│   ├── Governance/ # Conway-era governance (DReps, voting, proposals)
│   ├── Transaction/# Transaction building and signing
│   └── Query/      # Blockchain queries
├── EraIndependent/ # Era-agnostic utilities (keys, addresses, hashing)
├── Compatible/     # Backward compatibility layer
└── Legacy/         # Legacy Shelley commands
```

## Development Workflows

### Building and Testing

- **Standard build**: `cabal build all --enable-tests`
- **Run tests**: `cabal test all --enable-tests`
- **Golden tests**: Located in `cardano-cli/test/cardano-cli-golden/`, these capture CLI output and ensure stability
- **Property tests**: In `cardano-cli/test/cardano-cli-test/`, using Hedgehog for property-based testing

### Package Management

- **CHaP Repository**: Primary source for Cardano packages. Update with `nix flake lock --update-input CHaP`
- **Hackage Updates**: Bump `index-state` in `cabal.project`, then run `nix flake lock --update-input haskellNix/hackage`
- **Source Repository Packages**: Avoid long-term SRPs in `cabal.project`. Use temporarily, then release to CHaP

### Testing Patterns

- **Test Discovery**: Uses `tasty-discover` with `{-# OPTIONS_GHC -F -pgmF tasty-discover #-}` header
- **Property Tests**: Use `watchdogProp . propertyOnce` wrapper for timeout protection
- **CLI Testing**: Use `execCardanoCLI` from `Test.Cardano.CLI.Util` for command execution
- **Golden Files**: Expected outputs in `test/cardano-cli-golden/files/golden/`

## Project-Specific Conventions

### Language Extensions

Always include when using `MonadBaseControl`:
```haskell
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Trans.Control (MonadBaseControl)
```

### Error Handling

- Use `Cardano.Api` types for blockchain-specific errors
- Leverage `ExceptT`-based error handling throughout the codebase
- CLI errors should provide actionable user messages

### Command Structure

Each command follows this pattern:
1. **Parser** in `*Command.hs` - defines CLI options using `optparse-applicative`
2. **Runner** in `*Run.hs` - implements the command logic
3. **Types** in `*Option.hs` - shared option parsers and types

### CI/HLS Considerations

- **HLS Workflow**: The `.github/workflows/hls.yml` runs HLS checks. Ensure all dependencies in `cabal.project` are available to HLS
- **Source Repository Packages**: If adding temporary SRPs, ensure they include proper commit hashes and are documented as temporary
- **Test Discovery**: HLS may have issues with `tasty-discover` if wrong version is used - check CI for export errors

## Common Integration Points

- **Cardano API**: Primary interface to blockchain functionality via `cardano-api` package
- **Byron Legacy**: Separate command tree for Byron-era operations
- **Configuration Files**: Located in `configuration/cardano/` for mainnet settings
- **Protocol Parameters**: Fetched dynamically or from golden files for testing

## CLI Help Reference System

The codebase maintains comprehensive CLI help documentation as golden test files that capture exact command usage patterns:

### Help File Structure
- **Main help**: `cardano-cli/test/cardano-cli-golden/files/golden/help.cli` - Complete CLI tree
- **Command-specific**: `cardano-cli/test/cardano-cli-golden/files/golden/help/**/*.cli` - Individual commands

### Using Help Files for Command Generation
When generating CLI commands, reference these patterns:

**Era-based commands**: Most commands follow `cardano-cli [era] [category] [action]` pattern:
```bash
cardano-cli conway governance drep registration-certificate
cardano-cli conway transaction build
cardano-cli conway query protocol-parameters
```

**Key conventions**:
- Network selection: `--mainnet` or `--testnet-magic NATURAL`
- File inputs/outputs: Most flags use `--*-file FILEPATH` pattern
- Output formats: `--output-json`, `--output-yaml`, `--output-text`
- Socket path: `--socket-path SOCKET_PATH` (can use `CARDANO_NODE_SOCKET_PATH` env var)

**Command structure patterns**:
- **Key generation**: Always requires both `--verification-key-file` and `--signing-key-file`
- **Certificate creation**: Requires identity (key/hash) + deposit amount + `--out-file`
- **Transaction building**: Complex structure with inputs (`--tx-in`), outputs (`--tx-out`), change address
- **Queries**: Network + socket + optional tip selection + output format

### AI Assistant Guidelines for CLI Usage

1. **Always verify command syntax** against golden help files before suggesting commands
2. **Use era-specific commands** (`conway`, `babbage`) rather than generic ones when possible
3. **Include required network parameters** (`--mainnet`/`--testnet-magic`) for node interactions
4. **Specify file outputs** with `--out-file` when commands support it
5. **Reference environment variables** like `CARDANO_NODE_SOCKET_PATH` when applicable
6. **Group related operations** (e.g., key generation → certificate creation → transaction building)
7. **Use `--no-pager` with git commands** to prevent pager interference (e.g., `git --no-pager log`, `git --no-pager diff`, `git --no-pager show`)
8. **Use `.ai-scratch/` directory** for temporary files not intended to be committed (analysis outputs, debug files, test data)
9. **Write commit messages and PR descriptions** to new markdown files in `.ai-scratch/` when requested (e.g., `.ai-scratch/commit-message.md`, `.ai-scratch/pr-description.md`) - use unique filenames to avoid overwriting existing content
10. **Avoid `#` headings in commit messages** - Git treats lines starting with `#` as comments that will be ignored; use plain text formatting instead
11. **Use `.github/PULL_REQUEST_TEMPLATE.md` as template** for PR descriptions, filling in the changelog YAML, context, review guidance, and checklist
12. **Preserve commented changelog types** - When writing PR descriptions, leave unused changelog types commented out rather than removing them entirely; only uncomment the applicable types

## Architecture Decision Records (ADRs)

Architecture Decision Records document "architecturally significant" decisions that affect the structure, non-functional characteristics, dependencies, interfaces, or construction techniques of the cardano-cli project.

### What are ADRs?

ADRs are short documents that capture the motivation, context, and consequences of important architectural decisions. Each ADR follows a standard format with:

- **Status**: 📜 Proposed, ✅ Adopted, ❌ Rejected, 🗑️ Deprecated, or ⬆️ Superseded
- **Context**: The forces at play (technological, political, social, project-local) that led to the decision
- **Decision**: The response to these forces, stated clearly as "We will..."
- **Consequences**: All resulting effects, both positive and negative

### Consulting Local ADRs

The project maintains a local mirror of all ADRs in `docs/mirror/cardano-node-wiki/docs/`. Key ADRs for AI assistants include:

**Core Design Principles**:
- **ADR-001**: Default eras for CLI commands (Conway preference, Babbage compatibility)
- **ADR-004**: Support only mainnet and upcoming eras (deprecation strategy)
- **ADR-008**: Use RIO monad for IO operations instead of bare IO

**CLI Standards**:
- **ADR-007**: CLI output presentation (stdout for user data, stderr for messages)
- **ADR-009**: cardano-api module exports convention (qualified imports, explicit exports)
- **ADR-012**: Standardize CLI multiple choice flags (simple switches, `Vary` type)

**Development Practices**:
- **ADR-002**: Module structure for generators (Test.Gen.* naming conventions)
- **ADR-013**: Metavars must follow screaming snake case
- **ADR-014**: Total conversion functions conventions

### Using ADRs in Development

1. **Before making architectural changes**: Check existing ADRs in `docs/mirror/cardano-node-wiki/docs/`
2. **When encountering design patterns**: Look for the corresponding ADR to understand the rationale
3. **For new decisions**: Follow the ADR format from ADR-000 if creating new architectural decisions
4. **When updating**: Run `./scripts/wiki-update` to refresh the local ADR mirror

### AI Guidelines from ADRs
1. **Era handling**: Prefer Conway era commands, understand deprecation cycles
2. **Output design**: Route user data to stdout, progress/errors to stderr
3. **Flag patterns**: Use simple switches with clear defaults, avoid complex choice handling
4. **Module structure**: Use qualified imports and explicit exports in cardano-api integration
5. **Testing**: Follow RIO patterns for IO-based operations

### AI Assistant Wiki Management

**Before consulting ADRs or architectural decisions:**

1. **Check if wiki mirror exists**: If `docs/mirror/cardano-node-wiki/` doesn't exist, explain to the user:
   - "The cardano-node-wiki mirror directory is missing. This directory contains important architectural decision records (ADRs) and documentation that guide development decisions."
   - "Run `./scripts/wiki-update` to clone the latest version of the cardano-node-wiki repository locally."
   - "This ensures you have access to up-to-date architectural guidance and ADRs for the cardano-cli project."

2. **Monitor update freshness**: Check the timestamp in `docs/mirror/dates/cardano-node-wiki` to determine when the wiki was last updated:
   - If the file doesn't exist or is older than 7 days, suggest: "The local wiki mirror appears to be outdated (or missing). Run `./scripts/wiki-update` to fetch the latest architectural decisions and documentation."
   - If recently updated (within 7 days), proceed with referencing the local ADRs and documentation.

3. **When referencing ADRs**: Always use the local mirror paths (`docs/mirror/cardano-node-wiki/docs/ADR-*.md`) rather than external links to ensure consistency and availability.

## Quick Reference

- **Add new command**: Create parser in `EraBased/*Command.hs`, implement in `EraBased/*Run.hs`
- **Add CLI test**: Use `execCardanoCLI` in property test, update golden files if needed
- **Handle era transitions**: Use `AnyEraCommand` pattern with `Typeable` constraints
- **Debug CI issues**: Check `cabal.project` SRPs and ensure HLS can resolve all imports
- **Find command syntax**: Check `cardano-cli/test/cardano-cli-golden/files/golden/help/**/*.cli`
- **Consult ADRs**: Review architectural decisions in `docs/mirror/cardano-node-wiki/docs/ADR-*.md`
- **Update ADRs**: Run `./scripts/wiki-update` to refresh local ADR mirror from upstream
