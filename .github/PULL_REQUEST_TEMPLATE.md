# Changelog

```yaml
- description: |
    <insert-changelog-description-here>
# uncomment types applicable to the change:
  type:
  # - feature        # introduces a new feature
  # - breaking       # the API has changed in a breaking way
  # - compatible     # the API has changed but is non-breaking
  # - optimisation   # measurable performance improvements
  # - improvement    # QoL changes e.g. refactoring
  # - bugfix         # fixes a defect
  # - test           # fixes/modifies tests
  # - maintenance    # not directly related to the code
  # - release        # related to a new release preparation
  # - documentation  # change in code docs, haddocks...
```

# Context

Additional context for the PR goes here.

If the PR fixes a particular issue please provide a
[link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=)
to the issue.

# Checklist

- [ ] Commit sequence broadly makes sense and commits have useful messages
- [ ] The change log section in the PR description has been filled in
- [ ] New tests are added if needed and existing tests are updated.  These may include:
  - golden tests
  - property tests
  - round trip tests
  - integration tests
  See [Running tests](https://github.com/input-output-hk/cardano-node-wiki/wiki/Running-tests) for more details
- [ ] The version bounds in `.cabal` files are updated
- [ ] CI passes. See note on CI.  The following CI checks are required:
  - [ ] Code is linted with `hlint`.  See `.github/workflows/check-hlint.yml` to get the `hlint` version
  - [ ] Code is formatted with `stylish-haskell`.  See `.github/workflows/stylish-haskell.yml` to get the `stylish-haskell` version
  - [ ] Code builds on Linux, MacOS and Windows for `ghc-8.10.7` and `ghc-9.2.7`
- [ ] Self-reviewed the diff

<!-- 
### Note on CI ###
If your PR is from a fork, the necessary CI jobs won't trigger automatically for security reasons.
You will need to get someone with write privileges. Please contact IOG node developers to do this
for you. 
-->
