{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraBased.Constraints
  ( obtainIsShelleyBasedEraShelleyToBabbage
  , obtainIsShelleyBasedEraConwayOnwards
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

obtainIsShelleyBasedEraConwayOnwards :: ()
  => ConwayEraOnwards era
  -> ((IsShelleyBasedEra era, Ledger.EraCrypto (ShelleyLedgerEra era) ~ Ledger.StandardCrypto) => a)
  -> a
obtainIsShelleyBasedEraConwayOnwards = \case
  ConwayEraOnwardsConway -> id
