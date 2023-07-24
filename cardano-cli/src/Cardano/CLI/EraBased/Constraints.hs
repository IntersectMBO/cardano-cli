{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Constraints
  ( obtainIsShelleyBasedEraShelleyToBabbage
  , obtainConwayEraOnwardsConstraints
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

obtainIsShelleyBasedEraShelleyToBabbage :: ()
  => ShelleyToBabbageEra era
  -> (IsShelleyBasedEra era => a)
  -> a
obtainIsShelleyBasedEraShelleyToBabbage = \case
  ShelleyToBabbageEraShelley -> id
  ShelleyToBabbageEraAllegra -> id
  ShelleyToBabbageEraMary -> id
  ShelleyToBabbageEraAlonzo -> id
  ShelleyToBabbageEraBabbage -> id

obtainConwayEraOnwardsConstraints :: ()
  => ConwayEraOnwards era
  -> (  ( IsShelleyBasedEra era
        , Ledger.EraPParams (ShelleyLedgerEra era)
        , Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto) => a)
  -> a
obtainConwayEraOnwardsConstraints = \case
  ConwayEraOnwardsConway -> id
