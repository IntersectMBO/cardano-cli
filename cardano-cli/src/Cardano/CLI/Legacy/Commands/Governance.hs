{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Governance where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger
import           Cardano.Api.Shelley

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance
import           Cardano.CLI.Types.Key

import           Data.Foldable
import           Data.Text (Text)
import           Data.Word
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

data LegacyGovernanceCmds
  = GovernanceMIRPayStakeAddressesCertificate
      AnyShelleyToBabbageEra
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | GovernanceMIRTransfer
      AnyShelleyToBabbageEra
      Lovelace
      (File () Out)
      TransferDirection
  | GovernanceGenesisKeyDelegationCertificate
      AnyShelleyBasedEra
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)
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

renderLegacyGovernanceCmds :: LegacyGovernanceCmds -> Text
renderLegacyGovernanceCmds = \case
  GovernanceGenesisKeyDelegationCertificate {} -> "governance create-genesis-key-delegation-certificate"
  GovernanceMIRPayStakeAddressesCertificate {} -> "governance create-mir-certificate stake-addresses"
  GovernanceMIRTransfer _ _ _ TransferToTreasury -> "governance create-mir-certificate transfer-to-treasury"
  GovernanceMIRTransfer _ _ _ TransferToReserves -> "governance create-mir-certificate transfer-to-reserves"
  GovernanceUpdateProposal {} -> "governance create-update-proposal"
  GovernanceCreatePoll{} -> "governance create-poll"
  GovernanceAnswerPoll{} -> "governance answer-poll"
  GovernanceVerifyPoll{} -> "governance verify-poll"

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
      <*> pGovernanceActionId
      <*> pVotingCredential
      <*> (pShelleyBasedConway envCli <|> pure (AnyShelleyBasedEra ShelleyBasedEraConway))
      <*> pFileOutDirection "out-file" "Output filepath of the vote."

--------------------------------------------------------------------------------
-- Governance action related
--------------------------------------------------------------------------------

data ActionCmd
  = CreateConstitution
      Ledger.Network
      AnyShelleyBasedEra
      Lovelace
      (VerificationKeyOrFile StakePoolKey)
      (Maybe (TxId, Word32))
      ProposalUrl
      ProposalHashSource
      ConstitutionUrl
      ConstitutionHashSource
      (File ConstitutionText Out)

  deriving Show

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
  CreateConstitution
    <$> pNetwork
    <*> (pShelleyBasedConway envCli <|> pure (AnyShelleyBasedEra ShelleyBasedEraConway))
    <*> pGovActionDeposit
    <*> pVotingCredential
    <*> pPreviousGovernanceAction
    <*> pProposalUrl
    <*> pProposalHashSource
    <*> pConstitutionUrl
    <*> pConstitutionHashSource
    <*> pFileOutDirection "out-file" "Output filepath of the governance action."

pNetwork :: Parser Ledger.Network
pNetwork  = asum $ mconcat
  [ [ Opt.flag' Ledger.Mainnet $ mconcat
      [ Opt.long "mainnet"
      , Opt.help "Use the mainnet magic id."
      ]
    , Opt.flag' Ledger.Testnet $ mconcat
      [ Opt.long "testnet"
      , Opt.help "Use the testnet magic id."
      ]
    ]
  ]
