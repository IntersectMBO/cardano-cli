-- | Tests for the comparison behind @cardano-cli debug check-pool-registration@.
module Test.Cli.Debug.CheckPoolRegistration where

import Cardano.CLI.EraIndependent.Debug.CheckPoolRegistration.Internal.Diff

import Test.Cardano.CLI.Util (watchdogProp)

import Hedgehog (Property, (===))
import Hedgehog.Extras qualified as H

-- | Execute me with:
-- @cabal test cardano-cli-test --test-options '-p "/debug check pool registration covers every stake pool parameter/"'@
--
-- The comparison has to name every field of the ledger's @StakePoolParams@: a
-- field added in a future era must not be able to slip through unnoticed. The
-- field list the comparison works from is checked here against the one derived
-- from the type itself, so adding a field to the ledger breaks this test until
-- the comparison covers it.
hprop_debug_check_pool_registration_covers_every_stake_pool_parameter :: Property
hprop_debug_check_pool_registration_covers_every_stake_pool_parameter =
  watchdogProp . H.propertyOnce $
    comparedFieldNames === stakePoolParamsFieldNames
