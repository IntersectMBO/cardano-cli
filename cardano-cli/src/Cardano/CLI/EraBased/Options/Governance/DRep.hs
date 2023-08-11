{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance.DRep
  ( pGovernanceDRepCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Legacy (pKeyRegistDeposit,
                   pStakePoolRegistrationParserRequirements)
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Key

import           Data.Foldable
import           Data.String
import           Options.Applicative
import qualified Options.Applicative as Opt

pGovernanceDRepCmds :: EnvCli
                    -> CardanoEra era
                    -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepCmds envCli era = do
  subInfoParser "drep"
    (Opt.progDesc "Delegate Representative commands.")
    [ pGovernanceDRepRegistrationCertificate envCli era
    , pGovernanceDRepDelegationCertificate envCli era
    , pGovernanceDRepKeyGen era
    ]

pGovernanceDRepRegistrationCertificate :: EnvCli
                                       -> CardanoEra era
                                       -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepRegistrationCertificate envCli era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "registration-certificate"
    $ Opt.info (parser envCli w)
    $ Opt.progDesc "Create a registration certificate."
 where
  parser :: EnvCli -> AnyEraDecider era -> Parser (GovernanceDRepCmds era)
  parser envCli' = \case
    AnyEraDeciderShelleyToBabbage sToB ->
      GovernanceDRepRegistrationCertificate
        <$> asum [ ShelleyToBabbageStakePoolRegTarget sToB
                     <$> pStakePoolRegistrationParserRequirements envCli'
                 , ShelleyToBabbageStakeKeyRegTarget sToB
                     <$> pStakeIdentifier
                 ]
        <*> pOutputFile

    AnyEraDeciderConwayOnwards cOn ->
      GovernanceDRepRegistrationCertificate . ConwayOnwardRegTarget cOn
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


pGovernanceDRepDelegationCertificate :: EnvCli
                                     -> CardanoEra era
                                     -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepDelegationCertificate _envCli era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "delegation-certificate"
    $ Opt.info (parser w)
    $ Opt.progDesc "Delegation certificate creation."
 where
  parser :: AnyEraDecider era -> Parser (GovernanceDRepCmds era)
  parser w =
    GovernanceDRepDelegationCertificate
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

pGovernanceDRepKeyGen :: CardanoEra era
                      -> Maybe (Parser (GovernanceDRepCmds era))
pGovernanceDRepKeyGen era = do
  (AnyEraDeciderConwayOnwards _) <- maybeFeatureInEra era
  pure
    $ subParser "key-gen"
    $ Opt.info parser
    $ Opt.progDesc "Generate Delegate Representative verification and signing keys."
  where
    parser =
      GovernanceDRepKeyGen
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

-- TODO: Conway era AFTER sancho net. We probably want to
-- differentiate between delegating voting stake and reward stake
pStakeTarget :: ConwayEraOnwards era
             -> Parser (StakeTarget era)
pStakeTarget cOnwards =
  asum
    [ TargetStakePool cOnwards <$> pStakePoolVerificationKeyOrHashOrFile
    , TargetVotingDrep cOnwards <$> pDRepVerificationKeyOrHashOrFile
    , TargetVotingDrepAndStakePool cOnwards
         <$> pDRepVerificationKeyOrHashOrFile
         <*> pStakePoolVerificationKeyOrHashOrFile
    , TargetAlwaysAbstain cOnwards <$ pAlwaysAbstain
    , TargetAlwaysNoConfidence cOnwards <$ pAlwaysNoConfidence
   -- TODO: Conway era - necessary constructor not exposed by ledger yet
   -- so this option is hidden
    , TargetVotingDRepScriptHash cOnwards <$> pDRepScriptHash
    ]

pAlwaysAbstain :: Parser ()
pAlwaysAbstain =
  flag' () $ mconcat [ long "always-abstain"
                     , help "Abstain from voting on all proposals."
                     ]

pAlwaysNoConfidence :: Parser ()
pAlwaysNoConfidence =
  flag' () $ mconcat [ long "always-no-confidence"
                     , help "Always vote no confidence"
                     ]

pDRepScriptHash :: Parser ScriptHash
pDRepScriptHash =
  Opt.option scriptHashReader $ mconcat
    [ Opt.long "drep-script-hash"
    , Opt.metavar "HASH"
    , Opt.help $ mconcat
        [ "DRep script hash (hex-encoded).  "
        ]
    , Opt.hidden
    ]

scriptHashReader :: ReadM ScriptHash
scriptHashReader = eitherReader $ Right . fromString

