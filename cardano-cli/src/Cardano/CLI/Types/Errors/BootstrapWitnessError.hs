module Cardano.CLI.Types.Errors.BootstrapWitnessError
  ( BootstrapWitnessError (..)
  , renderBootstrapWitnessError
  )
where

import           Prettyprinter

-- | Error constructing a Shelley bootstrap witness (i.e. a Byron key witness
-- in the Shelley era).
data BootstrapWitnessError
  = -- | Neither a network ID nor a Byron address were provided to construct the
    -- Shelley bootstrap witness. One or the other is required.
    MissingNetworkIdOrByronAddressError
  deriving Show

-- | Render an error message for a 'BootstrapWitnessError'.
renderBootstrapWitnessError :: BootstrapWitnessError -> Doc ann
renderBootstrapWitnessError MissingNetworkIdOrByronAddressError =
  "Transactions witnessed by a Byron signing key must be accompanied by a "
    <> "network ID. Either provide a network ID or provide a Byron "
    <> "address with each Byron signing key (network IDs can be derived "
    <> "from Byron addresses)."
