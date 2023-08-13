{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.Governance
  ( parseGovernanceCmds
  ) where

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
parseGovernanceCmds :: EnvCli -> Parser LegacyGovernanceCmds
parseGovernanceCmds envCli =
  asum
    [ subParser "create-mir-certificate"
        $ Opt.info (pLegacyMIRPayStakeAddresses <|> mirCertParsers)
        $ Opt.progDesc "Create an MIR (Move Instantaneous Rewards) certificate"
    , subParser "create-genesis-key-delegation-certificate"
        $ Opt.info pGovernanceGenesisKeyDelegationCertificate
        $ Opt.progDesc "Create a genesis key delegation certificate"
    , subParser "create-update-proposal"
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
    mirCertParsers :: Parser LegacyGovernanceCmds
    mirCertParsers = asum
      [ subParser "stake-addresses"
        $ Opt.info pLegacyMIRPayStakeAddresses
        $ Opt.progDesc "Create an MIR certificate to pay stake addresses"
      , subParser "transfer-to-treasury"
        $ Opt.info pLegacyMIRTransferToTreasury
        $ Opt.progDesc "Create an MIR certificate to transfer from the reserves pot to the treasury pot"
      , subParser "transfer-to-rewards"
        $ Opt.info pLegacyMIRTransferToReserves
        $ Opt.progDesc "Create an MIR certificate to transfer from the treasury pot to the reserves pot"
      ]

    pLegacyMIRPayStakeAddresses :: Parser LegacyGovernanceCmds
    pLegacyMIRPayStakeAddresses =
      GovernanceMIRPayStakeAddressesCertificate
        <$> pAnyShelleyToBabbageEra envCli
        <*> pMIRPot
        <*> some pStakeAddress
        <*> some pRewardAmt
        <*> pOutputFile

    pLegacyMIRTransferToTreasury :: Parser LegacyGovernanceCmds
    pLegacyMIRTransferToTreasury =
      GovernanceMIRTransfer
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile
        <*> pure TransferToTreasury

    pLegacyMIRTransferToReserves :: Parser LegacyGovernanceCmds
    pLegacyMIRTransferToReserves =
      GovernanceMIRTransfer
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile
        <*> pure TransferToReserves

    pGovernanceGenesisKeyDelegationCertificate :: Parser LegacyGovernanceCmds
    pGovernanceGenesisKeyDelegationCertificate =
      GovernanceGenesisKeyDelegationCertificate
        <$> pAnyShelleyBasedEra envCli
        <*> pGenesisVerificationKeyOrHashOrFile
        <*> pGenesisDelegateVerificationKeyOrHashOrFile
        <*> pVrfVerificationKeyOrHashOrFile
        <*> pOutputFile

    pUpdateProposal :: Parser LegacyGovernanceCmds
    pUpdateProposal =
      GovernanceUpdateProposal
        <$> pOutputFile
        <*> pEpochNoUpdateProp
        <*> some pGenesisVerificationKeyFile
        <*> pProtocolParametersUpdate
        <*> optional pCostModels

    pGovernanceCreatePoll :: Parser LegacyGovernanceCmds
    pGovernanceCreatePoll =
      GovernanceCreatePoll
        <$> pPollQuestion
        <*> some pPollAnswer
        <*> optional pPollNonce
        <*> pOutputFile

    pGovernanceAnswerPoll :: Parser LegacyGovernanceCmds
    pGovernanceAnswerPoll =
      GovernanceAnswerPoll
        <$> pPollFile
        <*> optional pPollAnswerIndex
        <*> optional pOutputFile

    pGovernanceVerifyPoll :: Parser LegacyGovernanceCmds
    pGovernanceVerifyPoll =
      GovernanceVerifyPoll
        <$> pPollFile
        <*> pPollTxFile
        <*> optional pOutputFile
