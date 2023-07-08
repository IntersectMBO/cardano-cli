{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Conway.Parsers where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Common.Parsers
import           Cardano.CLI.Conway.Types
import           Cardano.CLI.Shelley.Key
import           Cardano.CLI.Types
import           Cardano.Ledger.Shelley.TxBody (MIRPot)

import           Data.Foldable
import           Data.Text (Text)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt


data GovernanceCmd
  = GovernanceVoteCmd VoteCmd
  | GovernanceActionCmd ActionCmd
  | GovernanceMIRPayStakeAddressesCertificate
      AnyShelleyBasedEra
      MIRPot
      [StakeAddress]
      [Lovelace]
      (File () Out)
  | GovernanceMIRTransfer
      AnyShelleyBasedEra
      Lovelace
      (File () Out)
      TransferDirection
  | GovernanceGenesisKeyDelegationCertificate
      AnyShelleyBasedEra
      (VerificationKeyOrHashOrFile GenesisKey)
      (VerificationKeyOrHashOrFile GenesisDelegateKey)
      (VerificationKeyOrHashOrFile VrfKey)
      (File () Out)
  | GovernanceUpdateProposal (File () Out) EpochNo
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

renderGovernanceCmd :: GovernanceCmd -> Text
renderGovernanceCmd = \case
  GovernanceVoteCmd {} -> "governance vote"
  GovernanceActionCmd {} -> "governance action"
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

pVoteCommmands :: Parser VoteCmd
pVoteCommmands =
  asum
    [ subParser "create-vote"
        $ Opt.info pCreateVote
        $ Opt.progDesc "Create a vote for a proposed governance action."
    ]

newtype VoteCmd
  = CreateVoteCmd ConwayVote deriving Show


pCreateVote :: Parser VoteCmd
pCreateVote =
  fmap CreateVoteCmd $
    ConwayVote
      <$> pVoteChoice
      <*> pVoterType
      <*> pGoveranceActionIdentifier
      <*> pVotingCredential
      <*> (pShelleyBasedConway <|> pure (AnyShelleyBasedEra ShelleyBasedEraConway))
      <*> pFileOutDirection "out-file" "Output filepath of the vote."

 where
  pVoteChoice :: Parser VoteChoice
  pVoteChoice =
    asum
     [  flag' Yes $ long "yes"
     ,  flag' No $ long "no"
     ,  flag' Abst $ long "abstain"
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

pActionCommmands :: Parser ActionCmd
pActionCommmands =
  asum
    [ subParser "create-action"
        $ Opt.info pCreateAction
        $ Opt.progDesc "Create a governance action."
    ]

pCreateAction :: Parser ActionCmd
pCreateAction =
  asum [ subParser "create-constitution"
           $ Opt.info pCreateConstitution
           $ Opt.progDesc "Create a constitution."
       ]


pCreateConstitution :: Parser ActionCmd
pCreateConstitution =
  fmap CreateConstitution $
    NewConstitution
      <$> (pShelleyBasedConway <|> pure (AnyShelleyBasedEra ShelleyBasedEraConway))
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
