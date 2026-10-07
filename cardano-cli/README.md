# cardano-cli

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

[Dijkstra protected payments and Receiving witnesses](RECEIVING.md).

Pool-state queries retain their existing non-BLS JSON fields. The query exposes
the BLS public key and possession proof, but omits its registration history, so
`spsBlsKey.bksRegisteredIn` is `null` when a BLS key is present.
