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
          [ "Query commands."
          ]
    )
    [ Just
        $ subParser "protocol-parameters"
        $ Opt.info (pQueryProtocolParameters envCli)
        $ Opt.progDesc "Get the node's current protocol parameters"
    , Just
        $ subParser "constitution-hash"
        $ Opt.info (pQueryConstitutionHash envCli)
        $ Opt.progDesc "Get the constitution hash"
    , Just
        $ subParser "tip"
        $ Opt.info (pQueryTip envCli)
        $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)"
    , Just
        $ subParser "stake-pools"
        $ Opt.info (pQueryStakePools envCli)
        $ Opt.progDesc "Get the node's current set of stake pool ids"
    , Just
        $ subParser "stake-distribution"
        $ Opt.info (pQueryStakeDistribution envCli)
        $ Opt.progDesc "Get the node's current aggregated stake distribution"
    , Just
        $ subParser "stake-address-info"
        $ Opt.info (pQueryStakeAddressInfo envCli)
        $ Opt.progDesc $ mconcat
            [ "Get the current delegations and reward accounts filtered by stake address."
            ]
    , Just
        $ subParser "utxo"
        $ Opt.info (pQueryUTxO envCli)
        $ Opt.progDesc $ mconcat
            [ "Get a portion of the current UTxO: by tx in, by address or the whole."
            ]
    , Just
        $ subParser "ledger-state"
        $ Opt.info (pQueryLedgerState envCli)
        $ Opt.progDesc $ mconcat
            [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
            ]
    , Just
        $ subParser "protocol-state"
        $ Opt.info (pQueryProtocolState envCli)
        $ Opt.progDesc $ mconcat
            [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
            ]
    , Just
        $ subParser "stake-snapshot"
        $ Opt.info (pQueryStakeSnapshot envCli)
        $ Opt.progDesc $ mconcat
            [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
            ]
    , Just
        $ hiddenSubParser "pool-params"
        $ Opt.info (pQueryPoolState envCli)
        $ Opt.progDesc $ mconcat
            [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
            , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
            ]
    , Just
        $ subParser "leadership-schedule"
        $ Opt.info (pLeadershipSchedule envCli)
        $ Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"
    , Just
        $ subParser "kes-period-info"
        $ Opt.info (pKesPeriodInfo envCli)
        $ Opt.progDesc "Get information about the current KES period and your node's operational certificate."
    , Just
        $ subParser "pool-state"
        $ Opt.info (pQueryPoolState envCli)
        $ Opt.progDesc "Dump the pool state"
    , Just
        $ subParser "tx-mempool"
        $ Opt.info (pQueryTxMempool envCli)
        $ Opt.progDesc "Local Mempool info"
    , Just
        $ subParser "slot-number"
        $ Opt.info (pQuerySlotNumber envCli)
        $ Opt.progDesc "Query slot number for UTC timestamp"
    ]

pQueryProtocolParameters :: EnvCli -> Parser (QueryCmds era)
pQueryProtocolParameters envCli =
  QueryProtocolParameters'
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryConstitutionHash :: EnvCli -> Parser (QueryCmds era)
pQueryConstitutionHash envCli =
  QueryConstitutionHash
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryTip :: EnvCli -> Parser (QueryCmds era)
pQueryTip envCli =
  QueryTip
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryUTxO :: EnvCli -> Parser (QueryCmds era)
pQueryUTxO envCli =
  QueryUTxO'
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pQueryUTxOFilter
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryStakePools :: EnvCli -> Parser (QueryCmds era)
pQueryStakePools envCli =
  QueryStakePools'
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryStakeDistribution :: EnvCli -> Parser (QueryCmds era)
pQueryStakeDistribution envCli =
  QueryStakeDistribution'
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryStakeAddressInfo :: EnvCli -> Parser (QueryCmds era)
pQueryStakeAddressInfo envCli =
  QueryStakeAddressInfo
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pFilterByStakeAddress
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryLedgerState :: EnvCli -> Parser (QueryCmds era)
pQueryLedgerState envCli =
  QueryDebugLedgerState'
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pMaybeOutputFile

pQueryProtocolState :: EnvCli -> Parser (QueryCmds era)
pQueryProtocolState envCli =
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
        pOnly = Only <$> many (pStakePoolVerificationKeyHash Nothing)

pQueryStakeSnapshot :: EnvCli -> Parser (QueryCmds era)
pQueryStakeSnapshot envCli =
  QueryStakeSnapshot'
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pAllStakePoolsOrOnly
    <*> pMaybeOutputFile

pQueryPoolState :: EnvCli -> Parser (QueryCmds era)
pQueryPoolState envCli =
  QueryPoolState'
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> many (pStakePoolVerificationKeyHash Nothing)

pQueryTxMempool :: EnvCli -> Parser (QueryCmds era)
pQueryTxMempool envCli =
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
pLeadershipSchedule :: EnvCli -> Parser (QueryCmds era)
pLeadershipSchedule envCli =
  QueryLeadershipSchedule
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pGenesisFile "Shelley genesis filepath"
    <*> pStakePoolVerificationKeyOrHashOrFile Nothing
    <*> pVrfSigningKeyFile
    <*> pWhichLeadershipSchedule
    <*> pMaybeOutputFile

pKesPeriodInfo :: EnvCli -> Parser (QueryCmds era)
pKesPeriodInfo envCli =
  QueryKesPeriodInfo
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pOperationalCertificateFile
    <*> pMaybeOutputFile

pQuerySlotNumber :: EnvCli -> Parser (QueryCmds era)
pQuerySlotNumber envCli =
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
