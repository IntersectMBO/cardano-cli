{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance
  ( EraBasedGovernanceCmds(..)
  , renderEraBasedGovernanceCmds
  , pEraBasedGovernanceCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Commands.Governance
import           Cardano.CLI.EraBased.Commands.Governance.DRep
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Governance.Actions
import           Cardano.CLI.EraBased.Options.Governance.Committee
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Data.Foldable
import           Data.Maybe
import           Data.String
import           Options.Applicative
import qualified Options.Applicative as Opt

-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pEraBasedGovernanceCmds :: EnvCli -> CardanoEra era -> Parser (EraBasedGovernanceCmds era)
pEraBasedGovernanceCmds envCli era =
  asum $ catMaybes
    [ pEraBasedVoteCmd envCli era
    , pCreateMirCertificatesCmds era
    , pGovernanceCommitteeCmds era <&> fmap EraBasedGovernanceCommitteeCmds
    , fmap EraBasedGovernanceActionCmds <$> pGovernanceActionCmds era
    , fmap EraBasedGovernanceDRepCmds   <$> pEraBasedDelegationCertificateCmd envCli era
    , fmap EraBasedGovernanceDRepCmds   <$> pEraBasedRegistrationCertificateCmd envCli era
    , fmap EraBasedGovernanceDRepCmds   <$> pDRepCommands era
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

--------------------------------------------------------------------------------

-- Vote related

pEraBasedVoteCmd
  :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmds era))
pEraBasedVoteCmd envCli era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "vote"
    $ Opt.info (pEraCmd' envCli w)
    $ Opt.progDesc "Vote creation."
 where
  pEraCmd'
    :: EnvCli -> ConwayEraOnwards era -> Parser (EraBasedGovernanceCmds era)
  pEraCmd' _envCli cOn =
      EraBasedGovernanceVoteCmd
        <$> pAnyVote cOn
        <*> pOutputFile

pAnyVote :: ConwayEraOnwards era -> Parser AnyVote
pAnyVote cOnwards =
  ConwayOnwardsVote cOnwards
    <$> pVoteChoice
    <*> pGoveranceActionIdentifier "TxIn of governance action (already on chain)."
    <*> pAnyVotingStakeVerificationKeyOrHashOrFile

pAnyVotingStakeVerificationKeyOrHashOrFile :: Parser AnyVotingStakeVerificationKeyOrHashOrFile
pAnyVotingStakeVerificationKeyOrHashOrFile =
  asum [ AnyDRepVerificationKeyOrHashOrFile <$> pDRepVerificationKeyOrHashOrFile
       , AnyStakePoolVerificationKeyOrHashOrFile <$> pStakePoolVerificationKeyOrHashOrFile
       ]



--------------------------------------------------------------------------------


pCreateMirCertificatesCmds :: CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmds era))
pCreateMirCertificatesCmds era = do
  w <- maybeFeatureInEra era
  pure
    $ subParser "create-mir-certificate"
    $ Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w)
    $ Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmds era)
mirCertParsers w =
  asum
    [ subParser "stake-addresses"
      $ Opt.info (pMIRPayStakeAddresses w)
      $ Opt.progDesc "Create an MIR certificate to pay stake addresses"
    , subParser "transfer-to-treasury"
      $ Opt.info (pMIRTransferToTreasury w)
      $ Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
    , subParser "transfer-to-rewards"
      $ Opt.info (pMIRTransferToReserves w)
      $ Opt.progDesc "Create an MIR certificate to transfer from the treasury pot to the reserves pot"
    ]

pMIRPayStakeAddresses :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmds era)
pMIRPayStakeAddresses w =
  EraBasedGovernanceMIRPayStakeAddressesCertificate w
    <$> pMIRPot
    <*> some pStakeAddress
    <*> some pRewardAmt
    <*> pOutputFile

pMIRTransferToTreasury :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmds era)
pMIRTransferToTreasury w =
  EraBasedGovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToTreasury

pMIRTransferToReserves :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmds era)
pMIRTransferToReserves w =
  EraBasedGovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToReserves

--------------------------------------------------------------------------------

pDRepCommands :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceDRepCmds era))
pDRepCommands era = do
  w <- maybeFeatureInEra era
  pure $
    subParser "drep"
    $ Opt.info (pKeyGen w)
    $ Opt.progDesc "Delegate Representative commands."
  where
    pKeyGen w =
      subParser "key-gen"
      $ Opt.info
          ( GovernanceDRepGenerateKey w
              <$> pVerificationKeyFileOut
              <*> pSigningKeyFileOut)
      $ Opt.progDesc "Generate Delegate Representative verification and signing keys."
