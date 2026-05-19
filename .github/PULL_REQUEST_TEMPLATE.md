<!--
Every PR needs a changelog fragment in `.changes/`.
Create one from the template at .changes/_TEMPLATE.yml, or use `herald` for interactive creation.
Run herald with nix: nix run github:input-output-hk/cardano-dev#herald

Interactive:
  herald new

Non-interactive:
  herald new -p cardano-cli -k bugfix -d "Fix something" --pr 1234

See .herald.yml for full configuration.
-->

# Context

Additional context for the PR goes here. If the PR fixes a particular issue please provide a [link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=) to the issue.

# How to trust this PR

Highlight important bits of the PR that will make the review faster. If there are commands the reviewer can run to observe the new behavior, describe them.

# Checklist

- [ ] Commit sequence broadly makes sense and commits have useful messages
- [ ] New tests are added if needed and existing tests are updated. See [Running tests](https://github.com/input-output-hk/cardano-node-wiki/wiki/Running-tests) for more details
- [ ] Self-reviewed the diff
- [ ] Changelog fragment added in `.changes/`

<!--
### Note on CI ###
If your PR is from a fork, the necessary CI jobs won't trigger automatically for security reasons.
You will need to get someone with write privileges. Please contact IOG node developers to do this
for you.
-->
