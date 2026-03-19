# GitHub Copilot Custom Instructions

## Haskell-Specific Guidance

### Qualified Import Aliases

In Haskell, it is valid and intentional to have multiple modules imported with the same qualified alias. For example:

```haskell
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
```

This is valid GHC Haskell — both modules can be referenced via the `Exp.` prefix. Name resolution happens at the use site; ambiguity only arises if both modules export the same identifier and it is used without disambiguation. Do **not** flag this pattern as a compile error or suggest renaming one of the aliases unless there is an actual ambiguity at a specific use site.

### Cabal File Formatting

In `.cabal` files, all top-level package metadata fields (`name:`, `version:`, `synopsis:`, `description:`, `copyright:`, `author:`, `maintainer:`, `license:`, `build-type:`, etc.) are written at column 0 with no leading indentation. This is the correct and standard Cabal format. Do **not** flag these fields as having inconsistent indentation or formatting issues — they are all intentionally at the same level.

When reviewing diffs of `.cabal` files, do **not** confuse diff context annotations (e.g., line numbers or `|` separators that appear in diff output) with actual file content. Only comment on the content of the file itself, not on artefacts of the diff format.
