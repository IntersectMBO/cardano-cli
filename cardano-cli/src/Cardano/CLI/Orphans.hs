{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Orphans () where

import           Cardano.Api (CardanoEra (..), FeatureInEra (..), ShelleyBasedEra (..))

instance FeatureInEra ShelleyBasedEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyBasedEraShelley
    AllegraEra  -> yes ShelleyBasedEraAllegra
    MaryEra     -> yes ShelleyBasedEraMary
    AlonzoEra   -> yes ShelleyBasedEraAlonzo
    BabbageEra  -> yes ShelleyBasedEraBabbage
    ConwayEra   -> yes ShelleyBasedEraConway
