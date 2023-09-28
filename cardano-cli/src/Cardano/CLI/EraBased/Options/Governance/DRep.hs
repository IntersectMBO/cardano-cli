{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.CLI.EraBased.Options.Governance.DRep
  ( pGovernanceDRepCmds
  ) where

import Cardano.Api

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Commands.Governance.DRep
import Cardano.CLI.EraBased.Options.Common
import Cardano.CLI.Parser
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Key

import Control.Applicative
import Data.Foldable
import Data.String
import Options.Applicative (Parser)
import Options.Applicative qualified as Opt

pGovernanceDRepCmds
  :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepCmds era envCli =
  subInfoParser
    "drep"
    ( Opt.progDesc $
        mconcat
          [ "DRep member commands."
          ]
    )
    [ pGovernanceDRepKeyGenCmd era
    , pGovernanceDRepKeyIdCmd era
    , pRegistrationCertificateCmd era envCli
    ]

pGovernanceDRepKeyGenCmd
  :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyGenCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "key-gen"
    $ Opt.info
      ( GovernanceDRepGenerateKeyCmd w
          <$> pVerificationKeyFileOut
          <*> pSigningKeyFileOut
      )
    $ Opt.progDesc "Generate Delegate Representative verification and signing keys."

pGovernanceDRepKeyIdCmd
  :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyIdCmd era = do
  w <- maybeEonInEra era
  pure
    $ subParser "id"
    $ Opt.info
      ( GovernanceDRepIdCmd w
          <$> pDRepVerificationKeyOrFile
          <*> pDRepIdOutputFormat
          <*> optional pOutputFile
      )
    $ Opt.progDesc "Generate a drep id."

pDRepIdOutputFormat :: Parser IdOutputFormat
pDRepIdOutputFormat =
  Opt.option readIdOutputFormat $
    mconcat
      [ Opt.long "output-format"
      , Opt.metavar "STRING"
      , Opt.help $
          mconcat
            [ "Optional drep id output format. Accepted output formats are \"hex\" "
            , "and \"bech32\" (default is \"bech32\")."
            ]
      , Opt.value IdOutputFormatBech32
      ]

-- Registration Certificate related

pRegistrationCertificateCmd
  :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (GovernanceDRepCmds era))
pRegistrationCertificateCmd era envCli = do
  w <- maybeEonInEra era
  pure $
    subParser "registration-certificate" $
      Opt.info (pEraCmd envCli w) $
        Opt.progDesc "Create a registration certificate."
 where
  pEraCmd :: EnvCli -> AnyEraDecider era -> Parser (GovernanceDRepCmds era)
  pEraCmd envCli' = \case
    AnyEraDeciderShelleyToBabbage sToB ->
      GovernanceDRepRegistrationCertificateCmd
        <$> asum
          [ ShelleyToBabbageStakePoolRegTarget sToB
              <$> pStakePoolRegistrationParserRequirements envCli'
          , ShelleyToBabbageStakeKeyRegTarget sToB
              <$> pStakeIdentifier
          ]
        <*> pOutputFile
    AnyEraDeciderConwayOnwards cOn ->
      GovernanceDRepRegistrationCertificateCmd . ConwayOnwardRegTarget cOn
        <$> asum
          [ RegisterStakePool cOn
              <$> pStakePoolRegistrationParserRequirements envCli'
          , RegisterStakeKey cOn
              <$> pStakeIdentifier
              <*> pKeyRegistDeposit
          , RegisterDRep cOn
              <$> pDRepVerificationKeyOrHashOrFile
              <*> pKeyRegistDeposit
          ]
        <*> pOutputFile

--------------------------------------------------------------------------------

data AnyEraDecider era where
  AnyEraDeciderShelleyToBabbage :: ShelleyToBabbageEra era -> AnyEraDecider era
  AnyEraDeciderConwayOnwards :: ConwayEraOnwards era -> AnyEraDecider era

instance Eon AnyEraDecider where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraShelley
    AllegraEra -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAllegra
    MaryEra -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraMary
    AlonzoEra -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAlonzo
    BabbageEra -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraBabbage
    ConwayEra -> yes $ AnyEraDeciderConwayOnwards ConwayEraOnwardsConway
