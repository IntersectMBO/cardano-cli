{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.EraBased.Options.Governance
  ( ActionCmd(..)
  , GovernanceCmd(..)
  , VoteCmd(..)
  , renderGovernanceCmd
  , pGovernanceCmd
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key
import           Cardano.CLI.Types.Legacy
import qualified Cardano.Ledger.Shelley.TxBody as Shelley

import           Data.Foldable
import           Data.Maybe
import           Data.Text (Text)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

data GovernanceCmd era
  = GovernanceMIRPayStakeAddressesCertificate
      (ShelleyToBabbageEra era)
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | GovernanceMIRTransfer
      (ShelleyToBabbageEra era)
      Lovelace
      (File () Out)
      TransferDirection
  | GovernanceDelegationCertificateCmd
      StakeIdentifier
      AnyDelegationTarget
      (File () Out)
  | GovernanceGenesisKeyDelegationCertificate
      (ShelleyToBabbageEra era)
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)
  | GovernanceVoteCmd
      VoteCmd
  | GovernanceActionCmd
      ActionCmd
  | GovernanceUpdateProposal
      (File () Out) EpochNo
      [VerificationKeyFile In]
      ProtocolParametersUpdate
      (Maybe FilePath)
  | GovernanceCreatePoll
      Text -- Prompt
      [Text] -- Choices
      (Maybe Word) -- Nonce
      (File GovernancePoll Out)
  | GovernanceAnswerPoll
      (File GovernancePoll In) -- Poll file
      (Maybe Word) -- Answer index
      (Maybe (File () Out)) -- Tx file
  | GovernanceVerifyPoll
      (File GovernancePoll In) -- Poll file
      (File (Tx ()) In) -- Tx file
      (Maybe (File () Out)) -- Tx file
  deriving Show

renderGovernanceCmd :: GovernanceCmd era -> Text
renderGovernanceCmd = \case
  GovernanceMIRPayStakeAddressesCertificate {} -> "TODO GovernanceMIRPayStakeAddressesCertificate"
  GovernanceMIRTransfer {} -> "TODO GovernanceMIRTransfer"
  GovernanceDelegationCertificateCmd {} -> "governance delegation-certificate"
  GovernanceGenesisKeyDelegationCertificate {} -> "TODO GovernanceGenesisKeyDelegationCertificate"
  GovernanceVoteCmd {} -> "governance vote"
  GovernanceActionCmd {} -> "governance action"
  GovernanceUpdateProposal {} -> "governance create-update-proposal"
  GovernanceCreatePoll{} -> "governance create-poll"
  GovernanceAnswerPoll{} -> "governance answer-poll"
  GovernanceVerifyPoll{} -> "governance verify-poll"

-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pGovernanceCmd :: EnvCli -> CardanoEra era -> Parser (GovernanceCmd era)
pGovernanceCmd envCli era =
  asum $ catMaybes
    [ pEraBasedDelegationCertificateCmd envCli era
    , pCreateMirCertificatesCmds era
    , pCreateGenesisKeyDelegationCertificateCmd era
    , pCreateUpdateProposalCmd
    , pCreatePollCmd
    , pAnswerPollCmd
    , pVerifyPollCmd
    , pVote envCli
    , pAction envCli
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

pEraBasedDelegationCertificateCmd :: EnvCli -> CardanoEra era -> Maybe (Parser (GovernanceCmd era))
pEraBasedDelegationCertificateCmd _envCli =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "delegation-certificate"
      $ Opt.info (pCmd w)
      $ Opt.progDesc "Post conway era governance command" -- TODO: We can render the help message based on the era
 where
  pCmd :: AnyEraDecider era -> Parser (GovernanceCmd era)
  pCmd w =
    GovernanceDelegationCertificateCmd
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
  -> Maybe (Parser (GovernanceCmd era))
pCreateGenesisKeyDelegationCertificateCmd =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "create-genesis-key-delegation-certificate"
      $ Opt.info (pGovernanceGenesisKeyDelegationCertificate w)
      $ Opt.progDesc "Create a genesis key delegation certificate"

pCreateMirCertificatesCmds :: ()
  => CardanoEra era
  -> Maybe (Parser (GovernanceCmd era))
pCreateMirCertificatesCmds =
  featureInEra Nothing $ \w ->
    Just
      $ subParser "create-mir-certificate"
      $ Opt.info (pMIRPayStakeAddresses w <|> mirCertParsers w)
      $ Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"

mirCertParsers :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmd era)
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
  -> Parser (GovernanceCmd era)
pMIRPayStakeAddresses w =
  GovernanceMIRPayStakeAddressesCertificate w
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
  -> Parser (GovernanceCmd era)
pMIRTransferToTreasury w =
  GovernanceMIRTransfer w
    <$> pTransferAmt
    <*> pOutputFile
    <*> pure TransferToTreasury

pMIRTransferToReserves :: ()
  => ShelleyToBabbageEra era
  -> Parser (GovernanceCmd era)
pMIRTransferToReserves w =
  GovernanceMIRTransfer w
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
  -> Parser (GovernanceCmd era)
pGovernanceGenesisKeyDelegationCertificate w =
  GovernanceGenesisKeyDelegationCertificate w
    <$> pGenesisVerificationKeyOrHashOrFile
    <*> pGenesisDelegateVerificationKeyOrHashOrFile
    <*> pVrfVerificationKeyOrHashOrFile
    <*> pOutputFile

pCreateUpdateProposalCmd :: Maybe (Parser (GovernanceCmd era))
pCreateUpdateProposalCmd =
  Just
    $ subParser "create-update-proposal"
    $ Opt.info pUpdateProposal
    $ Opt.progDesc "Create an update proposal"

pCreatePollCmd :: Maybe (Parser (GovernanceCmd era))
pCreatePollCmd =
  Just
    $ subParser "create-poll"
    $ Opt.info pGovernanceCreatePoll
    $ Opt.progDesc "Create an SPO poll"

pAnswerPollCmd :: Maybe (Parser (GovernanceCmd era))
pAnswerPollCmd =
  Just
    $ subParser "answer-poll"
    $ Opt.info pGovernanceAnswerPoll
    $ Opt.progDesc "Answer an SPO poll"

pVerifyPollCmd :: Maybe (Parser (GovernanceCmd era))
pVerifyPollCmd =
  Just
    $ subParser "verify-poll"
    $ Opt.info pGovernanceVerifyPoll
    $ Opt.progDesc "Verify an answer to a given SPO poll"

pVote :: EnvCli -> Maybe (Parser (GovernanceCmd era))
pVote envCli =
  Just
    $ fmap GovernanceVoteCmd $ subParser "vote"
    $ Opt.info (pVoteCommmands envCli)
    $ Opt.progDesc "Vote related commands."

pAction :: EnvCli -> Maybe (Parser (GovernanceCmd era))
pAction envCli =
  Just
    $ fmap GovernanceActionCmd $ subParser "action"
    $ Opt.info (pActionCommmands envCli)
    $ Opt.progDesc "Governance action related commands."

pUpdateProposal :: Parser (GovernanceCmd era)
pUpdateProposal =
  GovernanceUpdateProposal
    <$> pOutputFile
    <*> pEpochNoUpdateProp
    <*> some pGenesisVerificationKeyFile
    <*> pProtocolParametersUpdate
    <*> optional pCostModels

pGovernanceCreatePoll :: Parser (GovernanceCmd era)
pGovernanceCreatePoll =
  GovernanceCreatePoll
    <$> pPollQuestion
    <*> some pPollAnswer
    <*> optional pPollNonce
    <*> pOutputFile

pGovernanceAnswerPoll :: Parser (GovernanceCmd era)
pGovernanceAnswerPoll =
  GovernanceAnswerPoll
    <$> pPollFile
    <*> optional pPollAnswerIndex
    <*> optional pOutputFile

pGovernanceVerifyPoll :: Parser (GovernanceCmd era)
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

--------------------------------------------------------------------------------
-- Vote related
--------------------------------------------------------------------------------

pVoteCommmands :: EnvCli -> Parser VoteCmd
pVoteCommmands envCli =
  asum
    [ subParser "create-vote"
        $ Opt.info (pCreateVote envCli)
        $ Opt.progDesc "Create a vote for a proposed governance action."
    ]

newtype VoteCmd
  = CreateVoteCmd ConwayVote deriving Show


pCreateVote :: EnvCli -> Parser VoteCmd
pCreateVote envCli =
  fmap CreateVoteCmd $
    ConwayVote
      <$> pVoteChoice
      <*> pVoterType
      <*> pGoveranceActionIdentifier
      <*> pVotingCredential
      <*> (pShelleyBasedConway envCli <|> pure (AnyShelleyBasedEra ShelleyBasedEraConway))
      <*> pFileOutDirection "out-file" "Output filepath of the vote."

 where
  pVoteChoice :: Parser Vote
  pVoteChoice =
    asum
     [  flag' Yes $ long "yes"
     ,  flag' No $ long "no"
     ,  flag' Abstain $ long "abstain"
     ]

  pVoterType :: Parser VType
  pVoterType =
    asum
     [  flag' VCC $ mconcat [long "constitutional-committee-member", Opt.help "Member of the constiutional committee"]
     ,  flag' VDR $ mconcat [long "drep", Opt.help "Delegate representative"]
     ,  flag' VSP $ mconcat [long "spo", Opt.help "Stake pool operator"]
     ]

  pGoveranceActionIdentifier :: Parser TxIn
  pGoveranceActionIdentifier =
    Opt.option (readerFromParsecParser parseTxIn) $ mconcat
      [ Opt.long "tx-in"
      , Opt.metavar "TX-IN"
      , Opt.help "TxIn of governance action (already on chain)."
      ]

-- TODO: Conway era include "normal" stake keys
pVotingCredential :: Parser (VerificationKeyOrFile StakePoolKey)
pVotingCredential = pStakePoolVerificationKeyOrFile


--------------------------------------------------------------------------------
-- Governance action related
--------------------------------------------------------------------------------

newtype ActionCmd = CreateConstitution NewConstitution deriving Show

pActionCommmands :: EnvCli -> Parser ActionCmd
pActionCommmands envCli =
  asum
    [ subParser "create-action"
        $ Opt.info (pCreateAction envCli)
        $ Opt.progDesc "Create a governance action."
    ]

pCreateAction :: EnvCli -> Parser ActionCmd
pCreateAction envCli =
  asum
    [ subParser "create-constitution"
        $ Opt.info (pCreateConstitution envCli)
        $ Opt.progDesc "Create a constitution."
    ]


pCreateConstitution :: EnvCli -> Parser ActionCmd
pCreateConstitution envCli =
  fmap CreateConstitution $
    NewConstitution
      <$> (pShelleyBasedConway envCli <|> pure (AnyShelleyBasedEra ShelleyBasedEraConway))
      <*> pGovActionDeposit
      <*> pVotingCredential
      <*> pConstitution
      <*> pFileOutDirection "out-file" "Output filepath of the governance action."

pConstitution :: Parser Constitution
pConstitution =
  asum
    [ fmap ConstitutionFromText $ Opt.strOption $ mconcat
        [ Opt.long "constitution"
        , Opt.metavar "TEXT"
        , Opt.help "Input constitution as UTF-8 encoded text."
        ]
    , ConstitutionFromFile
        <$> pFileInDirection "constitution-file" "Input constitution as a text file."
    ]

pGovActionDeposit :: Parser Lovelace
pGovActionDeposit =
  Opt.option (readerFromParsecParser parseLovelace) $ mconcat
    [ Opt.long "governance-action-deposit"
    , Opt.metavar "NATURAL"
    , Opt.help "Deposit required to submit a governance action."
    ]
