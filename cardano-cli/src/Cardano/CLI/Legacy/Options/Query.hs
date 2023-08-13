{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.Query
  ( parseQueryCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.Query
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt

parseQueryCmds :: EnvCli -> Parser LegacyQueryCmds
parseQueryCmds envCli =
  asum
    [ subParser "protocol-parameters"
        $ Opt.info pQueryProtocolParameters
        $ Opt.progDesc "Get the node's current protocol parameters"
    , subParser "constitution-hash"
        $ Opt.info pQueryConstitutionHash
        $ Opt.progDesc "Get the constitution hash"
    , subParser "tip"
        $ Opt.info pQueryTip
        $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)"
    , subParser "stake-pools"
        $ Opt.info pQueryStakePools
        $ Opt.progDesc "Get the node's current set of stake pool ids"
    , subParser "stake-distribution"
        $ Opt.info pQueryStakeDistribution
        $ Opt.progDesc "Get the node's current aggregated stake distribution"
    , subParser "stake-address-info"
        $ Opt.info pQueryStakeAddressInfo
        $ Opt.progDesc $ mconcat
            [ "Get the current delegations and reward accounts filtered by stake address."
            ]
    , subParser "utxo"
        $ Opt.info pQueryUTxO
        $ Opt.progDesc $ mconcat
            [ "Get a portion of the current UTxO: by tx in, by address or the whole."
            ]
    , subParser "ledger-state"
        $ Opt.info pQueryLedgerState
        $ Opt.progDesc $ mconcat
            [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
            ]
    , subParser "protocol-state"
        $ Opt.info pQueryProtocolState
        $ Opt.progDesc $ mconcat
            [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
            ]
    , subParser "stake-snapshot"
        $ Opt.info pQueryStakeSnapshot
        $ Opt.progDesc $ mconcat
            [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
            ]
    , hiddenSubParser "pool-params"
        $ Opt.info pQueryPoolState
        $ Opt.progDesc $ mconcat
            [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
            , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
            ]
    , subParser "leadership-schedule"
        $ Opt.info pLeadershipSchedule
        $ Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"
    , subParser "kes-period-info"
        $ Opt.info pKesPeriodInfo
        $ Opt.progDesc "Get information about the current KES period and your node's operational certificate."
    , subParser "pool-state"
        $ Opt.info pQueryPoolState
        $ Opt.progDesc "Dump the pool state"
    , subParser "tx-mempool"
        $ Opt.info pQueryTxMempool
        $ Opt.progDesc "Local Mempool info"
    , subParser "slot-number"
        $ Opt.info pQuerySlotNumber
        $ Opt.progDesc "Query slot number for UTC timestamp"
    ]
  where
    pQueryProtocolParameters :: Parser LegacyQueryCmds
    pQueryProtocolParameters =
      QueryProtocolParameters'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryConstitutionHash :: Parser LegacyQueryCmds
    pQueryConstitutionHash =
      QueryConstitutionHash
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryTip :: Parser LegacyQueryCmds
    pQueryTip =
      QueryTip
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryUTxO :: Parser LegacyQueryCmds
    pQueryUTxO =
      QueryUTxO'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pQueryUTxOFilter
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakePools :: Parser LegacyQueryCmds
    pQueryStakePools =
      QueryStakePools'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakeDistribution :: Parser LegacyQueryCmds
    pQueryStakeDistribution =
      QueryStakeDistribution'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakeAddressInfo :: Parser LegacyQueryCmds
    pQueryStakeAddressInfo =
      QueryStakeAddressInfo
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pFilterByStakeAddress
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryLedgerState :: Parser LegacyQueryCmds
    pQueryLedgerState =
      QueryDebugLedgerState'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryProtocolState :: Parser LegacyQueryCmds
    pQueryProtocolState =
      QueryProtocolState'
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
            pOnly = Only <$> many pStakePoolVerificationKeyHash

    pQueryStakeSnapshot :: Parser LegacyQueryCmds
    pQueryStakeSnapshot =
      QueryStakeSnapshot'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pAllStakePoolsOrOnly
        <*> pMaybeOutputFile

    pQueryPoolState :: Parser LegacyQueryCmds
    pQueryPoolState =
      QueryPoolState'
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> many pStakePoolVerificationKeyHash

    pQueryTxMempool :: Parser LegacyQueryCmds
    pQueryTxMempool =
      QueryTxMempool
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
    pLeadershipSchedule :: Parser LegacyQueryCmds
    pLeadershipSchedule =
      QueryLeadershipSchedule
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pGenesisFile "Shelley genesis filepath"
        <*> pStakePoolVerificationKeyOrHashOrFile
        <*> pVrfSigningKeyFile
        <*> pWhichLeadershipSchedule
        <*> pMaybeOutputFile

    pKesPeriodInfo :: Parser LegacyQueryCmds
    pKesPeriodInfo =
      QueryKesPeriodInfo
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pOperationalCertificateFile
        <*> pMaybeOutputFile

    pQuerySlotNumber :: Parser LegacyQueryCmds
    pQuerySlotNumber =
      QuerySlotNumber
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
