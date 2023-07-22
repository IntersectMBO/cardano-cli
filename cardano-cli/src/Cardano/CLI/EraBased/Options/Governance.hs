{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance
  ( EraBasedGovernanceCmd(..)
  , renderEraBasedGovernanceCmd
  , pEraBasedGovernanceCmd
  , pGovernanceCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley (GovernancePoll (..), VrfKey)

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Governance
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Options.Applicative
import qualified Options.Applicative as Opt

data EraBasedGovernanceCmd era
  = EraBasedGovernancePreConwayCmd (ShelleyToBabbageEra era)
  | EraBasedGovernancePostConwayCmd (ConwayEraOnwards era)
  | EraBasedGovernanceMIRPayStakeAddressesCertificate
      (ShelleyToBabbageEra era)
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | EraBasedGovernanceMIRTransfer
      (ShelleyToBabbageEra era)
      Lovelace
      (File () Out)
      TransferDirection
  | EraBasedGovernanceDelegationCertificateCmd
      StakeIdentifier
      AnyDelegationTarget
      (File () Out)
  | EraBasedGovernanceGenesisKeyDelegationCertificate
      (ShelleyToBabbageEra era)
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)

renderEraBasedGovernanceCmd :: EraBasedGovernanceCmd era -> Text
renderEraBasedGovernanceCmd = \case
  EraBasedGovernancePreConwayCmd {} -> "governance pre-conway"
  EraBasedGovernancePostConwayCmd {} -> "governance post-conway"
  EraBasedGovernanceMIRPayStakeAddressesCertificate {} -> "TODO EraBasedGovernanceMIRPayStakeAddressesCertificate"
  EraBasedGovernanceMIRTransfer {} -> "TODO EraBasedGovernanceMIRTransfer"
  EraBasedGovernanceDelegationCertificateCmd {} -> "governance delegation-certificate"
  EraBasedGovernanceGenesisKeyDelegationCertificate {} -> "TODO EraBasedGovernanceGenesisKeyDelegationCertificate"

-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pEraBasedGovernanceCmd :: EnvCli -> CardanoEra era -> Parser (EraBasedGovernanceCmd era)
pEraBasedGovernanceCmd envCli era =
  asum $ catMaybes
    [ pEraBasedDelegationCertificateCmd envCli era
    , pCreateMirCertificatesCmds era
    , pCreateGenesisKeyDelegationCertificateCmd era
    ]

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

pEraBasedDelegationCertificateCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (EraBasedGovernanceCmd era))
pEraBasedDelegationCertificateCmd _envCli =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "delegation-certificate"
      $ Opt.info (pCmd w)
      $ Opt.progDesc "Post conway era governance command" -- TODO: We can render the help message based on the era
 where
  pCmd :: AnyEraDecider era -> Parser (EraBasedGovernanceCmd era)
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

pStakeTarget :: ConwayEraOnwards era -> Parser (StakeTarget era)
pStakeTarget cOnwards =
  asum
    [ TargetStakePool cOnwards <$> pStakePoolVerificationKeyOrHashOrFile
    , TargetVotingDrep cOnwards <$ pDrep
    -- , TargetVotingDrepAndStakePool cOnwards -- TODO: Conway era
    ]

pCreateGenesisKeyDelegationCertificateCmd :: ()
  => CardanoEra era
  -> Maybe (Parser (EraBasedGovernanceCmd era))
pCreateGenesisKeyDelegationCertificateCmd =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "create-genesis-key-delegation-certificate"
      $ Opt.info (pGovernanceGenesisKeyDelegationCertificate w)
      $ Opt.progDesc "Create a genesis key delegation certificate"

pCreateMirCertificatesCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (EraBasedGovernanceCmd era))
pCreateMirCertificatesCmds =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "create-mir-certificate"
      $ Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w)
      $ Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmd era)
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
  -> Parser (EraBasedGovernanceCmd era)
pMIRPayStakeAddresses w =
  EraBasedGovernanceMIRPayStakeAddressesCertificate w
    <$> pMIRPot
    <*> some pStakeAddress
    <*> some pRewardAmt
    <*> pOutputFile

pMIRPot :: Parser Shelley.MIRPot
pMIRPot =
  asum
    [ Opt.flag' Shelley.ReservesMIR $ mconcat
        [ Opt.long "reserves"
        , Opt.help "Use the reserves pot."
        ]
    , Opt.flag' Shelley.TreasuryMIR $ mconcat
        [ Opt.long "treasury"
        , Opt.help "Use the treasury pot."
        ]
    ]

pMIRTransferToTreasury :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmd era)
pMIRTransferToTreasury w =
  EraBasedGovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToTreasury

pMIRTransferToReserves :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmd era)
pMIRTransferToReserves w =
  EraBasedGovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToReserves

pTransferAmt :: Parser Lovelace
pTransferAmt =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "transfer"
    , Opt.metavar "LOVELACE"
    , Opt.help "The amount to transfer."
    ]

pRewardAmt :: Parser Lovelace
pRewardAmt =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "reward"
    , Opt.metavar "LOVELACE"
    , Opt.help "The reward for the relevant reward account."
    ]


{-
data DRep c
  = DRepKeyHash !(KeyHash 'Voting c)
  | DRepScriptHash !(ScriptHash c)
  | DRepAlwaysAbstain
  | DRepAlwaysNoConfidence
-}

-- TODO: Conway era - parse the relevant voting
-- credential (key hash, script hash, always abstain or no confidence)
pDrep :: Parser String
pDrep = Opt.strOption $ mconcat
          [ Opt.long "dummy-drep-option"
          , Opt.help "Delegate voting stake to Drep"
          ]

pGovernanceGenesisKeyDelegationCertificate :: ()
  => ShelleyToBabbageEra era
  -> Parser (EraBasedGovernanceCmd era)
pGovernanceGenesisKeyDelegationCertificate w =
  EraBasedGovernanceGenesisKeyDelegationCertificate w
    <$> pGenesisVerificationKeyOrHashOrFile
    <*> pGenesisDelegateVerificationKeyOrHashOrFile
    <*> pVrfVerificationKeyOrHashOrFile
    <*> pOutputFile

pGovernanceCmd :: EnvCli -> Parser GovernanceCmd
pGovernanceCmd envCli =
  asum
    [ subParser "create-update-proposal"
        $ Opt.info pUpdateProposal
        $ Opt.progDesc "Create an update proposal"
    , subParser "create-poll"
        $ Opt.info pGovernanceCreatePoll
        $ Opt.progDesc "Create an SPO poll"
    , subParser "answer-poll"
        $ Opt.info pGovernanceAnswerPoll
        $ Opt.progDesc "Answer an SPO poll"
    , subParser "verify-poll"
        $ Opt.info pGovernanceVerifyPoll
        $ Opt.progDesc "Verify an answer to a given SPO poll"
    , fmap GovernanceVoteCmd $ subParser "vote"
        $ Opt.info (pVoteCommmands envCli)
        $ Opt.progDesc "Vote related commands."
    , fmap GovernanceActionCmd $ subParser "action"
        $ Opt.info (pActionCommmands envCli)
        $ Opt.progDesc "Governance action related commands."
    ]
  where

    pUpdateProposal :: Parser GovernanceCmd
    pUpdateProposal =
      GovernanceUpdateProposal
        <$> pOutputFile
        <*> pEpochNoUpdateProp
        <*> some pGenesisVerificationKeyFile
        <*> pProtocolParametersUpdate
        <*> optional pCostModels

    pGovernanceCreatePoll :: Parser GovernanceCmd
    pGovernanceCreatePoll =
      GovernanceCreatePoll
        <$> pPollQuestion
        <*> some pPollAnswer
        <*> optional pPollNonce
        <*> pOutputFile

    pGovernanceAnswerPoll :: Parser GovernanceCmd
    pGovernanceAnswerPoll =
      GovernanceAnswerPoll
        <$> pPollFile
        <*> optional pPollAnswerIndex
        <*> optional pOutputFile

    pGovernanceVerifyPoll :: Parser GovernanceCmd
    pGovernanceVerifyPoll =
      GovernanceVerifyPoll
        <$> pPollFile
        <*> pPollTxFile
        <*> optional pOutputFile

pPollQuestion :: Parser Text
pPollQuestion =
  Opt.strOption $ mconcat
    [ Opt.long "question"
    , Opt.metavar "STRING"
    , Opt.help "The question for the poll."
    ]

pPollAnswer :: Parser Text
pPollAnswer =
  Opt.strOption $ mconcat
    [ Opt.long "answer"
    , Opt.metavar "STRING"
    , Opt.help "A possible choice for the poll. The option is repeatable."
    ]

pPollAnswerIndex :: Parser Word
pPollAnswerIndex =
  Opt.option auto $ mconcat
    [ Opt.long "answer"
    , Opt.metavar "INT"
    , Opt.help "The index of the chosen answer in the poll. Optional. Asked interactively if omitted."
    ]

pPollFile :: Parser (File GovernancePoll In)
pPollFile =
  Opt.strOption $ mconcat
    [ Opt.long "poll-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath to the ongoing poll."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pPollTxFile :: Parser (TxFile In)
pPollTxFile =
  fmap File $ Opt.strOption $ mconcat
    [ Opt.long "tx-file"
    , Opt.metavar "FILE"
    , Opt.help "Filepath to the JSON TxBody or JSON Tx carrying a valid poll answer."
    , Opt.completer (Opt.bashCompleter "file")
    ]

pPollNonce :: Parser Word
pPollNonce =
  Opt.option auto $ mconcat
    [ Opt.long "nonce"
    , Opt.metavar "UINT"
    , Opt.help "An (optional) nonce for non-replayability."
    ]
