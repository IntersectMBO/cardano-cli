{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Query
  ( pQueryCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Commands.Query
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pQueryCmds :: ()
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryCmds envCli =
  subInfoParser "query"
    ( Opt.progDesc
        $ mconcat
          [ "Node query commands. Will query the local node whose Unix domain socket is "
          , "obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
          ]
    )
    [ Just
        $ subParser "protocol-parameters"
        $ Opt.info (pQueryProtocolParametersCmd envCli)
        $ Opt.progDesc "Get the node's current protocol parameters"
    , Just
        $ subParser "constitution-hash"
        $ Opt.info (pQueryConstitutionHashCmd envCli)
        $ Opt.progDesc "Get the constitution hash"
    , Just
        $ subParser "tip"
        $ Opt.info (pQueryTipCmd envCli)
        $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)"
    , Just
        $ subParser "stake-pools"
        $ Opt.info (pQueryStakePoolsCmd envCli)
        $ Opt.progDesc "Get the node's current set of stake pool ids"
    , Just
        $ subParser "stake-distribution"
        $ Opt.info (pQueryStakeDistributionCmd envCli)
        $ Opt.progDesc "Get the node's current aggregated stake distribution"
    , Just
        $ subParser "stake-address-info"
        $ Opt.info (pQueryStakeAddressInfoCmd envCli)
        $ Opt.progDesc $ mconcat
            [ "Get the current delegations and reward accounts filtered by stake address."
            ]
    , Just
        $ subParser "utxo"
        $ Opt.info (pQueryUTxOCmd envCli)
        $ Opt.progDesc $ mconcat
            [ "Get a portion of the current UTxO: by tx in, by address or the whole."
            ]
    , Just
        $ subParser "ledger-state"
        $ Opt.info (pQueryLedgerStateCmd envCli)
        $ Opt.progDesc $ mconcat
            [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
            ]
    , Just
        $ subParser "protocol-state"
        $ Opt.info (pQueryProtocolStateCmd envCli)
        $ Opt.progDesc $ mconcat
            [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
            ]
    , Just
        $ subParser "stake-snapshot"
        $ Opt.info (pQueryStakeSnapshotCmd envCli)
        $ Opt.progDesc $ mconcat
            [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
            ]
    , Just
        $ hiddenSubParser "pool-params"
        $ Opt.info (pQueryPoolStateCmd envCli)
        $ Opt.progDesc $ mconcat
            [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
            , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
            ]
    , Just
        $ subParser "leadership-schedule"
        $ Opt.info (pLeadershipScheduleCmd envCli)
        $ Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"
    , Just
        $ subParser "kes-period-info"
        $ Opt.info (pKesPeriodInfoCmd envCli)
        $ Opt.progDesc "Get information about the current KES period and your node's operational certificate."
    , Just
        $ subParser "pool-state"
        $ Opt.info (pQueryPoolStateCmd envCli)
        $ Opt.progDesc "Dump the pool state"
    , Just
        $ subParser "tx-mempool"
        $ Opt.info (pQueryTxMempoolCmd envCli)
        $ Opt.progDesc "Local Mempool info"
    , Just
        $ subParser "slot-number"
        $ Opt.info (pQuerySlotNumberCmd envCli)
        $ Opt.progDesc "Query slot number for UTC timestamp"
    ]

pQueryProtocolParametersCmd :: EnvCli -> Parser (QueryCmds era)
pQueryProtocolParametersCmd envCli =
  fmap QueryProtocolParametersCmd $
    QueryProtocolParametersCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryConstitutionHashCmd :: EnvCli -> Parser (QueryCmds era)
pQueryConstitutionHashCmd envCli =
  fmap QueryConstitutionHashCmd $
    QueryConstitutionHashCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryTipCmd :: EnvCli -> Parser (QueryCmds era)
pQueryTipCmd envCli =
  fmap QueryTipCmd $
    QueryTipCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryUTxOCmd :: EnvCli -> Parser (QueryCmds era)
pQueryUTxOCmd envCli =
  fmap QueryUTxOCmd $
    QueryUTxOCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pQueryUTxOFilter
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryStakePoolsCmd :: EnvCli -> Parser (QueryCmds era)
pQueryStakePoolsCmd envCli =
  fmap QueryStakePoolsCmd $
    QueryStakePoolsCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryStakeDistributionCmd :: EnvCli -> Parser (QueryCmds era)
pQueryStakeDistributionCmd envCli =
  fmap QueryStakeDistributionCmd $
    QueryStakeDistributionCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryStakeAddressInfoCmd :: EnvCli -> Parser (QueryCmds era)
pQueryStakeAddressInfoCmd envCli =
  fmap QueryStakeAddressInfoCmd $
    QueryStakeAddressInfoCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pFilterByStakeAddress
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryLedgerStateCmd :: EnvCli -> Parser (QueryCmds era)
pQueryLedgerStateCmd envCli =
  fmap QueryLedgerStateCmd $
    QueryLedgerStateCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryProtocolStateCmd :: EnvCli -> Parser (QueryCmds era)
pQueryProtocolStateCmd envCli =
  fmap QueryProtocolStateCmd $
    QueryProtocolStateCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pAllStakePoolsOrOnly :: Parser (AllOrOnly [Hash StakePoolKey])
pAllStakePoolsOrOnly = pAll <|> pOnly
  where pAll :: Parser (AllOrOnly [Hash StakePoolKey])
        pAll = Opt.flag' All $ mconcat
          [ Opt.long "all-stake-pools"
          , Opt.help "Query for all stake pools"
          ]
        pOnly :: Parser (AllOrOnly [Hash StakePoolKey])
        pOnly = Only <$> many (pStakePoolVerificationKeyHash Nothing)

pQueryStakeSnapshotCmd :: EnvCli -> Parser (QueryCmds era)
pQueryStakeSnapshotCmd envCli =
  fmap QueryStakeSnapshotCmd $
    QueryStakeSnapshotCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pAllStakePoolsOrOnly
      <*> pMaybeOutputFile

pQueryPoolStateCmd :: EnvCli -> Parser (QueryCmds era)
pQueryPoolStateCmd envCli =
  fmap QueryPoolStateCmd $
    QueryPoolStateCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> many (pStakePoolVerificationKeyHash Nothing)

pQueryTxMempoolCmd :: EnvCli -> Parser (QueryCmds era)
pQueryTxMempoolCmd envCli =
  fmap QueryTxMempoolCmd $
    QueryTxMempoolCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTxMempoolQuery
      <*> pMaybeOutputFile
  where
    pTxMempoolQuery :: Parser TxMempoolQuery
    pTxMempoolQuery = asum
      [ subParser "info"
          $ Opt.info (pure TxMempoolQueryInfo)
          $ Opt.progDesc "Ask the node about the current mempool's capacity and sizes"
      , subParser "next-tx"
          $ Opt.info (pure TxMempoolQueryNextTx)
          $ Opt.progDesc "Requests the next transaction from the mempool's current list"
      , subParser "tx-exists"
          $ Opt.info (TxMempoolQueryTxExists <$> argument Opt.str (metavar "TX_ID"))
          $ Opt.progDesc "Query if a particular transaction exists in the mempool"
      ]
pLeadershipScheduleCmd :: EnvCli -> Parser (QueryCmds era)
pLeadershipScheduleCmd envCli =
  fmap QueryLeadershipScheduleCmd $
    QueryLeadershipScheduleCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pGenesisFile "Shelley genesis filepath"
      <*> pStakePoolVerificationKeyOrHashOrFile Nothing
      <*> pVrfSigningKeyFile
      <*> pWhichLeadershipSchedule
      <*> pMaybeOutputFile

pKesPeriodInfoCmd :: EnvCli -> Parser (QueryCmds era)
pKesPeriodInfoCmd envCli =
  fmap QueryKesPeriodInfoCmd $
    QueryKesPeriodInfoCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pOperationalCertificateFile
      <*> pMaybeOutputFile

pQuerySlotNumberCmd :: EnvCli -> Parser (QueryCmds era)
pQuerySlotNumberCmd envCli =
  fmap QuerySlotNumberCmd $
    QuerySlotNumberCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pUtcTimestamp
  where
    pUtcTimestamp =
      convertTime <$> (Opt.strArgument . mconcat)
        [ Opt.metavar "TIMESTAMP"
        , Opt.help "UTC timestamp in YYYY-MM-DDThh:mm:ssZ format"
        ]
