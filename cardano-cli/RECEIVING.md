# Dijkstra protected payments

These commands target the locally implemented CIP-160 contract. A node lifecycle
still needs a Dijkstra node with the agreed activation and Plutus V4 interface.

Protection is explicit. `address protect` preserves the payment credential,
network and stake reference of an existing base or enterprise address. Pointer
addresses cannot be protected. Inspect the result with `address info`.

```sh
cardano-cli dijkstra address protect --address "$ORDINARY_RECIPIENT" \
  --out-file recipient.addr
cardano-cli dijkstra address info --address "$(cat recipient.addr)"
```

A protected payment key recipient supplies an ordinary signature over the same
transaction body as the sender. Build the body once, share it with the recipient,
and use the existing witness and assembly commands. `debug transaction view`
displays `required recipient payment key witnesses (protected outputs)` separately
from script-required signers, so both parties can inspect the requirement.

```sh
cardano-cli dijkstra transaction build-raw --tx-in "$INPUT" \
  --tx-out "$(cat recipient.addr)+2000000" --fee 200000 --out-file payment.tx
cardano-cli debug transaction view --tx-body-file payment.tx --output-json
cardano-cli dijkstra transaction witness --tx-body-file payment.tx \
  --signing-key-file sender.skey --testnet-magic 42 --out-file sender.witness
cardano-cli dijkstra transaction witness --tx-body-file payment.tx \
  --signing-key-file recipient.skey --testnet-magic 42 --out-file recipient.witness
cardano-cli dijkstra transaction sign-witness --tx-body-file payment.tx \
  --witness-file sender.witness --witness-file recipient.witness --out-file payment.signed
```

Changing an output or its protection bit changes the signed body. Both parties
must inspect and sign the new body. The `receiving_key_recipient_multisigner_body_binding`
executable test checks both signatures against the agreed body, verifies assembly
preserves its hash, and rejects those signatures against a modified body.

For each protected Plutus output, supply `--receiving-output-index` with its
original zero-based position in the transaction body's ordinary outputs. Repeat
the option for separate outputs, even when they share a script hash. Each Plutus
Receiving invocation has its own redeemer and execution budget. Native witnesses
use `--receiving-script-file SCRIPT` without a redeemer or budget; supply the
script at an eligible output index. The same native script can authorize other
protected outputs with its hash through the existing script authorization rules.
Plutus Receiving requires V4, a redeemer and execution units for raw/offline estimated bodies:

```sh
cardano-cli dijkstra transaction policyid --script-file receiving.plutus
cardano-cli dijkstra transaction build-raw --tx-in "$INPUT" \
  --tx-out "$PROTECTED_SCRIPT_RECIPIENT+2000000" --tx-out-inline-datum-value 2 \
  --tx-in-collateral "$COLLATERAL" --protocol-params-file dijkstra-params.json \
  --receiving-output-index 0 --receiving-script-file receiving.plutus \
  --receiving-redeemer-value 0 --receiving-execution-units '(100000000,1000000)' \
  --fee 200000 --out-file receiving.tx
cardano-cli debug transaction view --tx-body-file receiving.tx --output-json
```

`transaction build` estimates execution units and fees against the node context.
`transaction build-estimate` accepts declared execution units, total input value
and witness counts for offline fee estimation. `--shelley-key-witnesses` is the
total number of distinct Shelley signatures you will assemble, including funding,
collateral, protected key recipients and native script requirements. A native
Receiving witness has no redeemer but its signature leaves still require witnesses.
For example, an ordinary funding key and a distinct native recipient key require
`--shelley-key-witnesses 2`. Include the native requirement when the script is in a
reference input: offline estimation has no UTxO from which to recover those keys.
Also supply the existing reference scripts' total serialized byte size via
`--reference-script-size`. Count shared signers once; a threshold script's chosen
satisfying signers must be included. Live `transaction build` resolves scripts
against the node UTxO and uses the API's conservative native-key bound.

`calculate-plutus-script-cost offline`
evaluates an existing body with supplied parameters, UTxO and era history. The test
fixture `test/cardano-cli-test/files/input/plutus/v4-receiving-even-datum.plutus`
comes from the generated ledger `receivingEvenDatum SPlutusV4` fixture.
The fixture checks the particular output passed to its Receiving invocation for
an even inline integer datum. Its regenerated per-output script hash is
`28d467557081773a051b5f83982574abfccceb079bc8012f192505e2`. The compiler generated
the fixture twice identically against the revised Plutus V4 interface. The CLI
regressions verify its hash, independent per-output redeemers and budgets, actual
execution-cost evaluation, and original output indexes through offline estimation
and change generation. All 81 unit tests and 820 goldens pass; a local-node
create/submit/spend lifecycle remains pending.

A native reference witness uses
`--receiving-simple-script-tx-in-reference TXID#INDEX`. A V4 reference witness uses
`--receiving-tx-in-reference TXID#INDEX --receiving-plutus-script-v4`,
`--receiving-reference-tx-in-redeemer-value DATA`, and
`--receiving-reference-tx-in-execution-units '(STEPS,MEMORY)'` for raw/estimate.
The referenced script must already be available in a consumed or reference input;
a new output's reference script is not available. Automatically derived references
that are already consumed stay in the consumed input set. Explicit reference input
requests retain the normal validation rules.

Receiving pointers are the raw output indexes: ordinary, protected key and native
outputs retain their positions and do not acquire Plutus Receiving redeemers.
A transaction with Plutus protected outputs at positions 1 and 4 has Receiving
pointers 1 and 4, including when both outputs share one script hash. Reordering
or removing outputs changes their indexes and requires rebuilding the witnesses
before signing. The CLI validates each witness against the selected protected
output's destination hash.

A protected script change output is appended after the authored outputs. If there
are two `--tx-out` values, provide its witness at index 2. Offline estimation
includes prospective change at that position and retains each output's declared
budget. Collateral return must use an unprotected address; specify an ordinary
return when using protected change. An automatically omitted change output has
no Receiving invocation.

The current CLI has no nested transaction construction command. Child body
construction and the `(child transaction ID, purpose index)` execution report are
available in the experimental API. These commands construct top bodies and retain
the existing CLI workflow; they do not invent a second child identifier format.
