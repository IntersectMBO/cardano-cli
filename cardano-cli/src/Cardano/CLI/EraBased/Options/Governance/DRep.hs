{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.CLI.EraBased.Options.Governance.DRep
  ( pGovernanceDRepCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Control.Applicative
import           Data.Foldable
import           Data.String
import           Options.Applicative (Parser)
import qualified Options.Applicative as Opt

pGovernanceDRepCmds :: ()
  => EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepCmds envCli era =
  subInfoParser "drep"
    ( Opt.progDesc
        $ mconcat
          [ "DRep member commands."
          ]
    )
    [ pGovernanceDRepKeyGen era
    , pGovernanceDRepKeyId era
    , pEraBasedDelegationCertificateCmd envCli era
    , pEraBasedRegistrationCertificateCmd envCli era
    ]

pGovernanceDRepKeyGen :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyGen era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "key-gen"
    $ Opt.info
        ( GovernanceDRepGenerateKey w
            <$> pVerificationKeyFileOut
            <*> pSigningKeyFileOut
        )
    $ Opt.progDesc "Generate Delegate Representative verification and signing keys."

pGovernanceDRepKeyId :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyId era = do
  w <- maybeFeatureInEra era
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
  Opt.option readIdOutputFormat $ mconcat
    [ Opt.long "output-format"
    , Opt.metavar "STRING"
    , Opt.help $ mconcat
      [ "Optional drep id output format. Accepted output formats are \"hex\" "
      , "and \"bech32\" (default is \"bech32\")."
      ]
    , Opt.value IdOutputFormatBech32
    ]

-- Registration Certificate related

pEraBasedRegistrationCertificateCmd :: ()
  => EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pEraBasedRegistrationCertificateCmd envCli era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "registration-certificate"
    $ Opt.info (pEraCmd envCli w)
    $ Opt.progDesc "Create a registration certificate."
 where
  pEraCmd :: EnvCli -> AnyEraDecider era -> Parser (GovernanceDRepCmds era)
  pEraCmd envCli' = \case
    AnyEraDeciderShelleyToBabbage sToB ->
      GovernanceDRepRegistrationCertificateCmd
        <$> asum [ ShelleyToBabbageStakePoolRegTarget sToB
                     <$> pStakePoolRegistrationParserRequirements envCli'
                 , ShelleyToBabbageStakeKeyRegTarget sToB
                     <$> pStakeIdentifier
                 ]
        <*> pOutputFile

    AnyEraDeciderConwayOnwards cOn ->
      GovernanceDRepRegistrationCertificateCmd . ConwayOnwardRegTarget cOn
        <$> asum [ RegisterStakePool cOn
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

instance FeatureInEra AnyEraDecider where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraShelley
    AllegraEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAllegra
    MaryEra     -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraMary
    AlonzoEra   -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes $ AnyEraDeciderShelleyToBabbage ShelleyToBabbageEraBabbage
    ConwayEra   -> yes $ AnyEraDeciderConwayOnwards ConwayEraOnwardsConway

-- Delegation Certificate related

pEraBasedDelegationCertificateCmd :: ()
  => EnvCli
  -> CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pEraBasedDelegationCertificateCmd _envCli era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "delegation-certificate"
    $ Opt.info (pCmd w)
    $ Opt.progDesc "Delegation certificate creation."
 where
  pCmd :: AnyEraDecider era -> Parser (GovernanceDRepCmds era)
  pCmd w =
    GovernanceDRepDelegationCertificateCmd
      <$> pStakeIdentifier
      <*> pAnyDelegationCertificateTarget w
      <*> pOutputFile

  pAnyDelegationCertificateTarget :: ()
    => AnyEraDecider era
    -> Parser AnyDelegationTarget
  pAnyDelegationCertificateTarget e =
    case e of
      AnyEraDeciderShelleyToBabbage sbe ->
        ShelleyToBabbageDelegTarget sbe
          <$> pStakePoolVerificationKeyOrHashOrFile
      AnyEraDeciderConwayOnwards cOnwards ->
        ConwayOnwardDelegTarget cOnwards
          <$> pStakeTarget cOnwards

-- TODO: Conway era AFTER sancho net. We probably want to
-- differentiate between delegating voting stake and reward stake
pStakeTarget :: ConwayEraOnwards era -> Parser (StakeTarget era)
pStakeTarget cOnwards =
  asum
    [ TargetVotingDrepAndStakePool cOnwards
        <$> pCombinedDRepVerificationKeyOrHashOrFile
        <*> pCombinedStakePoolVerificationKeyOrHashOrFile

    , TargetStakePool cOnwards <$> pStakePoolVerificationKeyOrHashOrFile

    , TargetVotingDrep cOnwards <$> pDRepVerificationKeyOrHashOrFile
    , TargetVotingDRepScriptHash cOnwards <$> pDRepScriptHash
    , TargetAlwaysAbstain cOnwards <$ pAlwaysAbstain

    , TargetAlwaysNoConfidence cOnwards <$ pAlwaysNoConfidence
    ]

pAlwaysAbstain :: Parser ()
pAlwaysAbstain =
  Opt.flag' () $ mconcat
    [ Opt.long "always-abstain"
    , Opt.help "Abstain from voting on all proposals."
    ]

pAlwaysNoConfidence :: Parser ()
pAlwaysNoConfidence =
  Opt.flag' () $ mconcat
    [ Opt.long "always-no-confidence"
    , Opt.help "Always vote no confidence"
    ]

pDRepScriptHash :: Parser ScriptHash
pDRepScriptHash =
  Opt.option scriptHashReader $ mconcat
    [ Opt.long "drep-script-hash"
    , Opt.metavar "HASH"
    , Opt.help $ mconcat
        [ "DRep script hash (hex-encoded).  "
        ]
    ]

scriptHashReader :: Opt.ReadM ScriptHash
scriptHashReader = Opt.eitherReader $ Right . fromString
