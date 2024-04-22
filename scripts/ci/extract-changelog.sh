#!/usr/bin/env bash
#
# Extracts the changelog of a specific version, by reading cardano-cli/CHANGELOG.md
#
# This script expects the version as first argument, for example "8.22.0.0"

set -o pipefail # We want commands with pipes to fail if any command in the stream fails

[[ -n "$1" ]] || { echo "This script expects a release version number as first and unique parameter. Please provide one (for example \"8.22.0\")"; exit 1; }

declare -r CHANGELOG="cardano-cli/CHANGELOG.md"

HEADER_LINE=$(grep -n "^## $1" "$CHANGELOG" | awk -F : '{print $1}')

# shellcheck disable=SC2181
[[ "$?" == "0" ]] || { echo "Release header not found (grep for \"^## $1\" failed)"; exit 1; }

CHANGELOG_START_LINE=$((HEADER_LINE + 2))

# Uncomment to debug
# echo "Found changelog starting line: $CHANGELOG_START_LINE"

AFTER_CHANGELOG_LINE=$((CHANGELOG_START_LINE + 1))
NUMBER_OF_LINES=$(tail -n "+$AFTER_CHANGELOG_LINE" $CHANGELOG | grep -n '^##' | head -n 1 | awk -F : '{print $1}')

# shellcheck disable=SC2181
[[ "$?" == "0" ]] || { echo "Next release header not found (grep for \"^## $1\" failed), starting at line $AFTER_CHANGELOG_LINE"; exit 1; }

NUMBER_OF_LINES=$((NUMBER_OF_LINES - 1))

# Uncomment to debug
# echo "Found number of lines: $NUMBER_OF_LINES"

# Piping tail doesn't play nice with "set -o pipefail" so turning it off
# See https://superuser.com/questions/554855/how-can-i-fix-a-broken-pipe-error
set +o pipefail

tail -n "+$CHANGELOG_START_LINE" "$CHANGELOG" | head -n "$NUMBER_OF_LINES"
