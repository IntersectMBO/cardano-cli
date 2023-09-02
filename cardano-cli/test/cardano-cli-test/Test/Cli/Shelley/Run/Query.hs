module Test.Cli.Shelley.Run.Query
  ( hprop_percentage
  ) where

import qualified Cardano.CLI.EraBased.Run.Query as Q
import           Cardano.Slotting.Time (RelativeTime (..))

import           Hedgehog (Property, (===))
import qualified Hedgehog.Extras.Test.Base as H

hprop_percentage :: Property
hprop_percentage = H.propertyOnce $ do
  Q.percentage (RelativeTime 10) (RelativeTime 1000) (RelativeTime 1000) === "100.00"
  Q.percentage (RelativeTime 10) (RelativeTime 990) (RelativeTime 1000) === "100.00"
  Q.percentage (RelativeTime 10) (RelativeTime 980) (RelativeTime 1000) === "99.00"
  Q.percentage (RelativeTime 10) (RelativeTime 500) (RelativeTime 1000) === "51.05"
  Q.percentage (RelativeTime 10) (RelativeTime 0) (RelativeTime 1000) === "1.10"
  return ()
