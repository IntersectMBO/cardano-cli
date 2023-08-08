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
import           Cardano.CLI.EraBased.Governance
import           Cardano.CLI.EraBased.Legacy
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.EraBased.Options.Governance.Actions
import           Cardano.CLI.EraBased.Options.Governance.Committee
import           Cardano.CLI.EraBased.Vote
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy

import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import           Data.String
import           Options.Applicative
import qualified Options.Applicative as Opt

-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pEraBasedGovernanceCmds :: EnvCli -> CardanoEra era -> Parser (EraBasedGovernanceCmds era)
pEraBasedGovernanceCmds envCli era =
  asum $ catMaybes
    [ pEraBasedRegistrationCertificateCmd envCli era
    , pEraBasedDelegationCertificateCmd envCli era
    , pEraBasedVoteCmd envCli era
    , pCreateMirCertificatesCmds era
    , pGovernanceCommitteeCmds era <&> fmap EraBasedGovernanceCommitteeCmds
    , fmap EraBasedGovernanceActionCmds <$> pGovernanceActionCmds era
    ]


-- Registration Certificate related

pEraBasedRegistrationCertificateCmd
  :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmds era))
pEraBasedRegistrationCertificateCmd envCli =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "registration-certificate"
      $ Opt.info (pEraCmd envCli w)
      $ Opt.progDesc "Create a registration certificate."
 where
  pEraCmd :: EnvCli -> AnyEraDecider era -> Parser (EraBasedGovernanceCmds era)
  pEraCmd envCli' = \case
    AnyEraDeciderShelleyToBabbage sToB ->
      EraBasedGovernanceRegistrationCertificateCmd
        <$> asum [ ShelleyToBabbageStakePoolRegTarget sToB
                     <$> pStakePoolRegistrationParserRequirements envCli'
                 , ShelleyToBabbageStakeKeyRegTarget sToB
                     <$> pStakeIdentifier
                 ]
        <*> pOutputFile

    AnyEraDeciderConwayOnwards cOn ->
      EraBasedGovernanceRegistrationCertificateCmd . ConwayOnwardRegTarget cOn
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

pEraBasedDelegationCertificateCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmds era))
pEraBasedDelegationCertificateCmd _envCli =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "delegation-certificate"
      $ Opt.info (pCmd w)
      $ Opt.progDesc "Delegation certificate creation."
 where
  pCmd :: AnyEraDecider era -> Parser (EraBasedGovernanceCmds era)
  pCmd w =
    EraBasedGovernanceDelegationCertificateCmd
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

pDRepVerificationKeyOrHashOrFile
  :: Parser (VerificationKeyOrHashOrFile DRepKey)
pDRepVerificationKeyOrHashOrFile =
  asum
    [ VerificationKeyOrFile <$> pDRepVerificationKeyOrFile
    , VerificationKeyHash <$> pDRepVerificationKeyHash
    ]

pDRepVerificationKeyHash :: Parser (Hash DRepKey)
pDRepVerificationKeyHash =
    Opt.option (pBech32KeyHash AsDRepKey <|> pHexHash AsDRepKey) $ mconcat
      [ Opt.long "drep-key-hash"
      , Opt.metavar "HASH"
      , Opt.help $ mconcat
          [ "DRep verification key hash (either Bech32-encoded or hex-encoded).  "
          , "Zero or more occurences of this option is allowed."
          ]
      ]

pDRepVerificationKey :: Parser (VerificationKey DRepKey)
pDRepVerificationKey =
  Opt.option (readVerificationKey AsDRepKey) $ mconcat
    [ Opt.long "drep-verification-key"
    , Opt.metavar "STRING"
    , Opt.help "DRep verification key (Bech32 or hex-encoded)."
    ]

pDRepVerificationKeyOrFile :: Parser (VerificationKeyOrFile DRepKey)
pDRepVerificationKeyOrFile =
  asum
    [ VerificationKeyValue <$> pDRepVerificationKey
    , VerificationKeyFilePath <$> pDRepVerificationKeyFile
    ]

pDRepVerificationKeyFile :: Parser (VerificationKeyFile In)
pDRepVerificationKeyFile =
  fmap File . Opt.strOption $ mconcat
    [ Opt.long "drep-verification-key-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath of the DRep verification key."
    , Opt.completer (Opt.bashCompleter "file")
    ]

--------------------------------------------------------------------------------

-- Vote related

pEraBasedVoteCmd
  :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmds era))
pEraBasedVoteCmd envCli =
  featureInEra Nothing $ \w ->
    Just
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
    <*> pGoveranceActionIdentifier
    <*> pAnyVotingStakeVerificationKeyOrHashOrFile

pAnyVotingStakeVerificationKeyOrHashOrFile :: Parser AnyVotingStakeVerificationKeyOrHashOrFile
pAnyVotingStakeVerificationKeyOrHashOrFile =
  asum [ AnyDRepVerificationKeyOrHashOrFile <$> pDRepVerificationKeyOrHashOrFile
       , AnyStakePoolVerificationKeyOrHashOrFile <$> pStakePoolVerificationKeyOrHashOrFile
       ]



--------------------------------------------------------------------------------


pCreateMirCertificatesCmds :: CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmds era))
pCreateMirCertificatesCmds =
  featureInEra Nothing $ \w ->
    Just
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

