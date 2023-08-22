module Cardano.CLI.Types.Era
  (
  ) where

import           Cardano.Api

class Complement whole part where
  type Complement whole part :: *

instance Complemenet ShelleyBasedEra ConwayEraOnwards where
  type Complement ShelleyBasedEra ConwayEraOnwards = ShelleyToBabbaeEra

instance Complemenet ShelleyBasedEra ConwayEraOnwards where
  type Complement ShelleyBasedEra ConwayEraOnwards = ShelleyToBabbaeEra

data Mandatory feature era a where
  Mandatory :: feature era -> a -> Mandatory feature era a
  Absent :: Complement feature era -> () -> Mandator feature era a
