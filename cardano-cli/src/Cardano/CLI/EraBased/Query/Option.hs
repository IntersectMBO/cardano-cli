{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Alternative law, left identity" -}
{- HLINT ignore "Move brackets to avoid $" -}
{- HLINT ignore "Use <$>" -}

module Cardano.CLI.EraBased.Query.Option
  ( pQueryCmds
  , pQueryCmdsTopLevel
  )
where

import Cardano.Api hiding (QueryInShelleyBasedEra (..))
import Cardano.Api qualified as MemberStatus (MemberStatus (..))
import Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Query.Command
import Cardano.CLI.Parser
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Data.Foldable
import GHC.Exts (IsList (..))
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt

pQueryCmdsTopLevel :: EnvCli -> Parser (QueryCmds ConwayEra)
pQueryCmdsTopLevel envCli =
  let parsers =
        [ pProtocolParams envCli
        , pTip envCli
        , pStakePools envCli
        , pStakeDistribution envCli
        , pStakeAddressInfo envCli
        , pUTxO envCli
        , pLedgerState envCli
        , pProtocolState envCli
        , pStakeSnapshot envCli
        , pPoolParams envCli
        , pLeadershipSchedule envCli
        , pKesPeriodInfo envCli
        , pPoolState envCli
        , pTxMempool envCli
        , pSlotNumber envCli
        , pQueryLedgerPeerSnapshot envCli
        ]
      i =
        Opt.progDesc $
          mconcat
            [ "Node query commands. Will query the local node whose Unix domain socket is "
            , "obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
   in Opt.hsubparser $
        commandWithMetavar "query" $
          Opt.info (asum parsers) i

pProtocolParams :: EnvCli -> Parser (QueryCmds era)
pProtocolParams envCli =
  Opt.hsubparser $
    commandWithMetavar "protocol-parameters" $
      Opt.info (pQueryProtocolParametersCmd envCli) $
        Opt.progDesc "Get the node's current protocol parameters"

pTip :: EnvCli -> Parser (QueryCmds ConwayEra)
pTip envCli =
  Opt.hsubparser $
    commandWithMetavar "tip" $
      Opt.info (pQueryTipCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc "Get the node's current tip (slot no, hash, block no)"

pStakePools :: EnvCli -> Parser (QueryCmds ConwayEra)
pStakePools envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-pools" $
      Opt.info (pQueryStakePoolsCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc "Get the node's current set of stake pool ids"

pStakeDistribution :: EnvCli -> Parser (QueryCmds ConwayEra)
pStakeDistribution envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-distribution" $
      Opt.info (pQueryStakeDistributionCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc "Get the node's current aggregated stake distribution"

pStakeAddressInfo :: EnvCli -> Parser (QueryCmds ConwayEra)
pStakeAddressInfo envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-address-info" $
      Opt.info (pQueryStakeAddressInfoCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc $
          mconcat
            [ "Get the current delegations and reward accounts filtered by stake address."
            ]

pUTxO :: EnvCli -> Parser (QueryCmds ConwayEra)
pUTxO envCli =
  Opt.hsubparser $
    commandWithMetavar "utxo" $
      Opt.info (pQueryUTxOCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc $
          mconcat
            [ "Get a portion of the current UTxO: by tx in, by address or the whole."
            ]

pLedgerState :: EnvCli -> Parser (QueryCmds ConwayEra)
pLedgerState envCli =
  Opt.hsubparser $
    commandWithMetavar "ledger-state" $
      Opt.info (pQueryLedgerStateCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc $
          mconcat
            [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
            ]

pProtocolState :: EnvCli -> Parser (QueryCmds ConwayEra)
pProtocolState envCli =
  Opt.hsubparser $
    commandWithMetavar "protocol-state" $
      Opt.info (pQueryProtocolStateCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc $
          mconcat
            [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
            ]

pStakeSnapshot :: EnvCli -> Parser (QueryCmds ConwayEra)
pStakeSnapshot envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-snapshot" $
      Opt.info (pQueryStakeSnapshotCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc $
          mconcat
            [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
            ]

pPoolParams :: EnvCli -> Parser (QueryCmds ConwayEra)
pPoolParams envCli =
  Opt.hsubparser $
    mconcat
      [ Opt.hidden
      , commandWithMetavar "pool-params" $
          Opt.info (pQueryPoolStateCmd ShelleyBasedEraConway envCli) $
            Opt.progDesc $
              mconcat
                [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
                , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
                ]
      ]

pLeadershipSchedule :: EnvCli -> Parser (QueryCmds ConwayEra)
pLeadershipSchedule envCli =
  Opt.hsubparser $
    commandWithMetavar "leadership-schedule" $
      Opt.info (pLeadershipScheduleCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"

pKesPeriodInfo :: EnvCli -> Parser (QueryCmds ConwayEra)
pKesPeriodInfo envCli =
  Opt.hsubparser $
    commandWithMetavar "kes-period-info" $
      Opt.info (pKesPeriodInfoCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc "Get information about the current KES period and your node's operational certificate."

pPoolState :: EnvCli -> Parser (QueryCmds ConwayEra)
pPoolState envCli =
  Opt.hsubparser $
    commandWithMetavar "pool-state" $
      Opt.info (pQueryPoolStateCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc "Dump the pool state"

pTxMempool :: EnvCli -> Parser (QueryCmds era)
pTxMempool envCli =
  Opt.hsubparser $
    commandWithMetavar "tx-mempool" $
      Opt.info (pQueryTxMempoolCmd envCli) $
        Opt.progDesc "Local Mempool info"

pSlotNumber :: EnvCli -> Parser (QueryCmds ConwayEra)
pSlotNumber envCli =
  Opt.hsubparser $
    commandWithMetavar "slot-number" $
      Opt.info (pQuerySlotNumberCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc "Query slot number for UTC timestamp"

pQueryLedgerPeerSnapshot :: EnvCli -> Parser (QueryCmds ConwayEra)
pQueryLedgerPeerSnapshot envCli =
  Opt.hsubparser $
    commandWithMetavar "ledger-peer-snapshot" $
      Opt.info (pQueryLedgerPeerSnapshotCmd ShelleyBasedEraConway envCli) $
        Opt.progDesc $
          mconcat
            [ "Dump the current snapshot of big ledger peers. "
            , "These are the largest pools that cumulatively hold "
            , "90% of total stake."
            ]

-- \^ TODO use bigLedgerPeerQuota from Ouroboros.Network.PeerSelection.LedgerPeers.Utils
-- which must be re-exposed thru cardano-api

pQueryCmds
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryCmds era envCli =
  subInfoParser
    "query"
    ( Opt.progDesc $
        mconcat
          [ "Node query commands. Will query the local node whose Unix domain socket is "
          , "obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
          ]
    )
    [ pQueryGetCommitteeStateCmd era envCli
    , pQueryGetConstitutionCmd era envCli
    , pQueryDRepStateCmd era envCli
    , pQueryDRepStakeDistributionCmd era envCli
    , pQueryEraHistoryCmd era envCli
    , pQueryFuturePParamsCmd era envCli
    , pQueryGetGovStateCmd era envCli
    , Just $
        Opt.hsubparser $
          commandWithMetavar "kes-period-info" $
            Opt.info (pKesPeriodInfoCmd era envCli) $
              Opt.progDesc "Get information about the current KES period and your node's operational certificate."
    , Just $
        Opt.hsubparser $
          commandWithMetavar "leadership-schedule" $
            Opt.info (pLeadershipScheduleCmd era envCli) $
              Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"
    , Just $
        Opt.hsubparser $
          commandWithMetavar "ledger-peer-snapshot" $
            Opt.info (pQueryLedgerPeerSnapshotCmd era envCli) $
              Opt.progDesc $
                mconcat
                  [ "Dump the current snapshot of ledger peers."
                  , "These are the largest pools that cumulatively hold "
                  , "90% of total stake."
                  ]
    , -- \^ TODO use bigLedgerPeerQuota from Ouroboros.Network.PeerSelection.LedgerPeers.Utils
      -- which must be re-exposed thru cardano-api
      Just $
        Opt.hsubparser $
          commandWithMetavar "ledger-state" $
            Opt.info (pQueryLedgerStateCmd era envCli) $
              Opt.progDesc $
                mconcat
                  [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
                  ]
    , Just $
        Opt.hsubparser $
          mconcat
            [ Opt.hidden
            , commandWithMetavar "pool-params" $
                Opt.info (pQueryPoolStateCmd era envCli) $
                  Opt.progDesc $
                    mconcat
                      [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
                      , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
                      ]
            ]
    , Just $
        Opt.hsubparser $
          commandWithMetavar "pool-state" $
            Opt.info (pQueryPoolStateCmd era envCli) $
              Opt.progDesc "Dump the pool state"
    , pQueryProposalsCmd era envCli
    , Just $
        Opt.hsubparser $
          commandWithMetavar "protocol-parameters" $
            Opt.info (pQueryProtocolParametersCmd envCli) $
              Opt.progDesc "Get the node's current protocol parameters"
    , Just $
        Opt.hsubparser $
          commandWithMetavar "protocol-state" $
            Opt.info (pQueryProtocolStateCmd era envCli) $
              Opt.progDesc $
                mconcat
                  [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
                  ]
    , pQueryGetRatifyStateCmd era envCli
    , Just
        . Opt.hsubparser
        . commandWithMetavar "ref-script-size"
        . Opt.info (pQueryRefScriptSizeCmd era envCli)
        $ Opt.progDesc "Calculate the reference input scripts size in bytes for provided transaction inputs."
    , Just $
        Opt.hsubparser $
          commandWithMetavar "slot-number" $
            Opt.info (pQuerySlotNumberCmd era envCli) $
              Opt.progDesc "Query slot number for UTC timestamp"
    , pQuerySPOStakeDistributionCmd era envCli
    , Just $
        Opt.hsubparser $
          commandWithMetavar "stake-address-info" $
            Opt.info (pQueryStakeAddressInfoCmd era envCli) $
              Opt.progDesc $
                mconcat
                  [ "Get the current delegations and reward accounts filtered by stake address."
                  ]
    , Just $
        Opt.hsubparser $
          commandWithMetavar "stake-distribution" $
            Opt.info (pQueryStakeDistributionCmd era envCli) $
              Opt.progDesc "Get the node's current aggregated stake distribution"
    , Just $
        Opt.hsubparser $
          commandWithMetavar "stake-pools" $
            Opt.info (pQueryStakePoolsCmd era envCli) $
              Opt.progDesc "Get the node's current set of stake pool ids"
    , pQueryStakePoolDefaultVote era envCli
    , Just $
        Opt.hsubparser $
          commandWithMetavar "stake-snapshot" $
            Opt.info (pQueryStakeSnapshotCmd era envCli) $
              Opt.progDesc $
                mconcat
                  [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
                  ]
    , Just $
        Opt.hsubparser $
          commandWithMetavar "tip" $
            Opt.info (pQueryTipCmd era envCli) $
              Opt.progDesc "Get the node's current tip (slot no, hash, block no)"
    , pQueryTreasuryValueCmd era envCli
    , Just $
        Opt.hsubparser $
          commandWithMetavar "tx-mempool" $
            Opt.info (pQueryTxMempoolCmd envCli) $
              Opt.progDesc "Local Mempool info"
    , Just $
        Opt.hsubparser $
          commandWithMetavar "utxo" $
            Opt.info (pQueryUTxOCmd era envCli) $
              Opt.progDesc $
                mconcat
                  [ "Get a portion of the current UTxO: by tx in, by address or the whole."
                  ]
    ]

pQueryProtocolParametersCmd :: EnvCli -> Parser (QueryCmds era)
pQueryProtocolParametersCmd envCli =
  fmap QueryProtocolParametersCmd $
    QueryProtocolParametersCmdArgs
      <$> ( LocalNodeConnectInfo
              <$> pConsensusModeParams
              <*> pNetworkId envCli
              <*> pSocketPath envCli
          )
      <*> pMaybeOutputFile

pQueryTipCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryTipCmd era envCli =
  fmap QueryTipCmd $
    QueryTipCmdArgs
      <$> pQueryCommons era envCli
      <*> pMaybeOutputFile

pQueryUTxOCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryUTxOCmd era envCli =
  fmap QueryUTxOCmd $
    QueryUTxOCmdArgs
      <$> pQueryCommons era envCli
      <*> pQueryUTxOFilter
      <*> ( optional $
              asum
                [ pFormatCBOR "utxo"
                , pFormatJsonFileDefault "utxo"
                , pFormatTextStdoutDefault "utxo"
                ]
          )
      <*> pMaybeOutputFile

pQueryStakePoolsCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakePoolsCmd era envCli =
  fmap QueryStakePoolsCmd $
    QueryStakePoolsCmdArgs
      <$> pQueryCommons era envCli
      <*> (optional $ pOutputFormatJsonOrText "stake-pools")
      <*> pMaybeOutputFile

pQueryStakeDistributionCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakeDistributionCmd era envCli =
  fmap QueryStakeDistributionCmd $
    QueryStakeDistributionCmdArgs
      <$> pQueryCommons era envCli
      <*> (optional $ pOutputFormatJsonOrText "stake-distribution")
      <*> pMaybeOutputFile

pQueryStakeAddressInfoCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakeAddressInfoCmd era envCli =
  fmap QueryStakeAddressInfoCmd $
    QueryStakeAddressInfoCmdArgs
      <$> pQueryCommons era envCli
      <*> pFilterByStakeAddress
      <*> pMaybeOutputFile

pQueryLedgerStateCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryLedgerStateCmd era envCli =
  fmap QueryLedgerStateCmd $
    QueryLedgerStateCmdArgs
      <$> pQueryCommons era envCli
      <*> pMaybeOutputFile

pQueryLedgerPeerSnapshotCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryLedgerPeerSnapshotCmd era envCli =
  fmap QueryLedgerPeerSnapshotCmd $
    QueryLedgerPeerSnapshotCmdArgs
      <$> pQueryCommons era envCli
      <*> pMaybeOutputFile

pQueryProtocolStateCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryProtocolStateCmd era envCli =
  fmap QueryProtocolStateCmd $
    QueryProtocolStateCmdArgs
      <$> pQueryCommons era envCli
      <*> pMaybeOutputFile

pAllStakePoolsOrSome :: Parser (AllOrOnly (Hash StakePoolKey))
pAllStakePoolsOrSome = pAll <|> pOnly
 where
  pAll :: Parser (AllOrOnly (Hash StakePoolKey))
  pAll =
    Opt.flag' All $
      mconcat
        [ Opt.long "all-stake-pools"
        , Opt.help "Query for all stake pools"
        ]
  pOnly :: Parser (AllOrOnly (Hash StakePoolKey))
  pOnly = Only <$> some (pStakePoolVerificationKeyHash Nothing)

pQueryStakeSnapshotCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakeSnapshotCmd era envCli =
  fmap QueryStakeSnapshotCmd $
    QueryStakeSnapshotCmdArgs
      <$> pQueryCommons era envCli
      <*> pAllStakePoolsOrSome
      <*> pMaybeOutputFile

pQueryPoolStateCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryPoolStateCmd era envCli =
  fmap QueryPoolStateCmd $
    QueryPoolStateCmdArgs
      <$> pQueryCommons era envCli
      <*> pAllStakePoolsOrSome
      <*> pMaybeOutputFile

pQueryTxMempoolCmd :: EnvCli -> Parser (QueryCmds era)
pQueryTxMempoolCmd envCli =
  fmap QueryTxMempoolCmd $
    QueryTxMempoolCmdArgs
      <$> ( LocalNodeConnectInfo
              <$> pConsensusModeParams
              <*> pNetworkId envCli
              <*> pSocketPath envCli
          )
      <*> pTxMempoolQuery
      <*> pMaybeOutputFile
 where
  pTxMempoolQuery :: Parser TxMempoolQuery
  pTxMempoolQuery =
    asum
      [ Opt.hsubparser $
          commandWithMetavar "info" $
            Opt.info (pure TxMempoolQueryInfo) $
              Opt.progDesc "Ask the node about the current mempool's capacity and sizes"
      , Opt.hsubparser $
          commandWithMetavar "next-tx" $
            Opt.info (pure TxMempoolQueryNextTx) $
              Opt.progDesc "Requests the next transaction from the mempool's current list"
      , Opt.hsubparser $
          commandWithMetavar "tx-exists" $
            Opt.info (TxMempoolQueryTxExists <$> argument Opt.str (metavar "TX_ID")) $
              Opt.progDesc "Query if a particular transaction exists in the mempool"
      ]

pLeadershipScheduleCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pLeadershipScheduleCmd era envCli =
  fmap QueryLeadershipScheduleCmd $
    QueryLeadershipScheduleCmdArgs
      <$> pQueryCommons era envCli
      <*> pGenesisFile "Shelley genesis filepath"
      <*> pStakePoolVerificationKeyOrHashOrFile Nothing
      <*> pVrfSigningKeyFile
      <*> pWhichLeadershipSchedule
      <*> (optional $ pOutputFormatJsonOrText "leadership-schedule")
      <*> pMaybeOutputFile

pKesPeriodInfoCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pKesPeriodInfoCmd era envCli =
  fmap QueryKesPeriodInfoCmd $
    QueryKesPeriodInfoCmdArgs
      <$> pQueryCommons era envCli
      <*> pOperationalCertificateFile
      <*> pMaybeOutputFile

pQuerySlotNumberCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQuerySlotNumberCmd era envCli =
  fmap QuerySlotNumberCmd $
    QuerySlotNumberCmdArgs
      <$> pQueryCommons era envCli
      <*> pUtcTimestamp
 where
  pUtcTimestamp =
    convertTime
      <$> (Opt.strArgument . mconcat)
        [ Opt.metavar "TIMESTAMP"
        , Opt.help "UTC timestamp in YYYY-MM-DDThh:mm:ssZ format"
        ]

pQueryRefScriptSizeCmd :: ShelleyBasedEra era -> EnvCli -> Parser (QueryCmds era)
pQueryRefScriptSizeCmd era envCli =
  fmap QueryRefScriptSizeCmd $
    QueryRefScriptSizeCmdArgs
      <$> pQueryCommons era envCli
      <*> (fromList <$> some pByTxIn)
      <*> (optional $ pOutputFormatJsonOrText "reference inputs")
      <*> pMaybeOutputFile
 where
  pByTxIn :: Parser TxIn
  pByTxIn =
    Opt.option (readerFromParsecParser parseTxIn) $
      mconcat
        [ Opt.long "tx-in"
        , Opt.metavar "TX-IN"
        , Opt.help "Transaction input (TxId#TxIx)."
        ]

pQueryGetConstitutionCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetConstitutionCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "constitution" $
        Opt.info (QueryConstitutionCmd <$> pQueryNoArgCmdArgs w envCli) $
          Opt.progDesc "Get the constitution"

pQueryGetGovStateCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetGovStateCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "gov-state" $
        Opt.info (QueryGovStateCmd <$> pQueryNoArgCmdArgs w envCli) $
          Opt.progDesc "Get the governance state"

pQueryGetRatifyStateCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetRatifyStateCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "ratify-state" $
        Opt.info (QueryRatifyStateCmd <$> pQueryNoArgCmdArgs w envCli) $
          Opt.progDesc "Get the ratification state"

pQueryFuturePParamsCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryFuturePParamsCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "future-pparams" $
        Opt.info (QueryFuturePParamsCmd <$> pQueryNoArgCmdArgs w envCli) $
          Opt.progDesc "Get the protocol parameters that will apply at the next epoch"

-- TODO Conway: DRep State and DRep Stake Distribution parsers use DRep keys to obtain DRep credentials. This only
-- makes use of 'KeyHashObj' constructor of 'Credential kr c'. Should we also support here 'ScriptHashObj'?
-- What about 'DRep c' - this means that only 'KeyHash' constructor is in use here: should also
-- 'DRepAlwaysAbstain' and 'DRepAlwaysNoConfidence' be supported here?

pQueryDRepStateCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryDRepStateCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "drep-state" $
        Opt.info (QueryDRepStateCmd <$> pQueryDRepStateCmdArgs w) $
          Opt.progDesc "Get the DRep state."
 where
  pQueryDRepStateCmdArgs :: ConwayEraOnwards era -> Parser (QueryDRepStateCmdArgs era)
  pQueryDRepStateCmdArgs w =
    QueryDRepStateCmdArgs w
      <$> pQueryCommons era envCli
      <*> pAllOrOnlyDRepHashSource
      <*> Opt.flag
        NoStake
        WithStake
        ( mconcat
            [ Opt.long "include-stake"
            , Opt.help $
                mconcat
                  [ "Also return the stake associated with each DRep. "
                  , "The result is the same as with \"drep-stake-distribution\"; "
                  , "this is a convenience option to obtain all information concerning a DRep at once. "
                  , "This is a potentially expensive query, so it's OFF by default."
                  ]
            ]
        )
      <*> pMaybeOutputFile

pQueryDRepStakeDistributionCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryDRepStakeDistributionCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "drep-stake-distribution" $
        Opt.info (QueryDRepStakeDistributionCmd <$> pQueryDRepStakeDistributionCmdArgs w) $
          Opt.progDesc "Get the DRep stake distribution."
 where
  pQueryDRepStakeDistributionCmdArgs
    :: ConwayEraOnwards era -> Parser (QueryDRepStakeDistributionCmdArgs era)
  pQueryDRepStakeDistributionCmdArgs w =
    QueryDRepStakeDistributionCmdArgs w
      <$> pQueryCommons era envCli
      <*> pAllOrOnlyDRepHashSource
      <*> pMaybeOutputFile

pQueryProposalsCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryProposalsCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "proposals" $
        Opt.info (QueryProposalsCmd <$> pQueryProposalsCmdArgs w) $
          Opt.progDesc $
            mconcat
              [ "Get the governance proposals that are eligible for ratification. "
              , "Proposals submitted during the current epoch are excluded, as they cannot be ratified until the next epoch. "
              ]
 where
  pQueryProposalsCmdArgs :: ConwayEraOnwards era -> Parser (QueryProposalsCmdArgs era)
  pQueryProposalsCmdArgs w =
    QueryProposalsCmdArgs w
      <$> pQueryCommons (convert w) envCli
      <*> pAllOrOnlyGovActionIds
      <*> optional pOutputFile

pQuerySPOStakeDistributionCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQuerySPOStakeDistributionCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "spo-stake-distribution" $
        Opt.info (QuerySPOStakeDistributionCmd <$> pQuerySPOStakeDistributionCmdArgs w) $
          Opt.progDesc "Get the SPO stake distribution."
 where
  pQuerySPOStakeDistributionCmdArgs
    :: ConwayEraOnwards era -> Parser (QuerySPOStakeDistributionCmdArgs era)
  pQuerySPOStakeDistributionCmdArgs w =
    QuerySPOStakeDistributionCmdArgs w
      <$> pQueryCommons era envCli
      <*> pAllOrOnlySPOHashSource
      <*> pMaybeOutputFile

pQueryGetCommitteeStateCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetCommitteeStateCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "committee-state" $
        Opt.info (QueryCommitteeMembersStateCmd <$> pQueryCommitteeMembersStateArgs w) $
          Opt.progDesc "Get the committee state"
 where
  pQueryCommitteeMembersStateArgs
    :: ConwayEraOnwards era -> Parser (QueryCommitteeMembersStateCmdArgs era)
  pQueryCommitteeMembersStateArgs w =
    QueryCommitteeMembersStateCmdArgs w
      <$> pQueryCommons era envCli
      <*> many pCommitteeColdVerificationKeyOrHashOrFileOrScriptHash
      <*> many pCommitteeHotKeyOrHashOrFileOrScriptHash
      <*> many pMemberStatus
      <*> pMaybeOutputFile

  pCommitteeColdVerificationKeyOrHashOrFileOrScriptHash
    :: Parser (VerificationKeyOrHashOrFileOrScriptHash CommitteeColdKey)
  pCommitteeColdVerificationKeyOrHashOrFileOrScriptHash =
    asum
      [ VkhfshKeyHashFile <$> pCommitteeColdVerificationKeyOrHashOrFile
      , VkhfshScriptHash
          <$> pScriptHash
            "cold-script-hash"
            "Cold Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
      ]

  pCommitteeHotKeyOrHashOrFileOrScriptHash
    :: Parser (VerificationKeyOrHashOrFileOrScriptHash CommitteeHotKey)
  pCommitteeHotKeyOrHashOrFileOrScriptHash =
    asum
      [ VkhfshKeyHashFile <$> pCommitteeHotKeyOrHashOrFile
      , VkhfshScriptHash
          <$> pScriptHash
            "hot-script-hash"
            "Hot Native or Plutus script file hash (hex-encoded). Obtain it with \"cardano-cli hash script ...\"."
      ]

  pMemberStatus :: Parser MemberStatus
  pMemberStatus =
    asum
      [ Opt.flag' MemberStatus.Active $
          mconcat
            [ Opt.long "active"
            , Opt.help "Active committee members (members whose vote will count during ratification)"
            ]
      , Opt.flag' MemberStatus.Expired $
          mconcat
            [ Opt.long "expired"
            , Opt.help "Expired committee members"
            ]
      , Opt.flag' MemberStatus.Unrecognized $
          mconcat
            [ Opt.long "unrecognized"
            , Opt.help "Unrecognized committe members: a hot credential for an unknown cold credential"
            ]
      ]

pQueryTreasuryValueCmd
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryTreasuryValueCmd era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "treasury" $
        Opt.info (QueryTreasuryValueCmd <$> pQueryTreasuryValueArgs w) $
          Opt.progDesc "Get the treasury value"
 where
  pQueryTreasuryValueArgs
    :: ConwayEraOnwards era -> Parser (QueryTreasuryValueCmdArgs era)
  pQueryTreasuryValueArgs w =
    QueryTreasuryValueCmdArgs w
      <$> pQueryCommons era envCli
      <*> pMaybeOutputFile

pQueryStakePoolDefaultVote
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryStakePoolDefaultVote era envCli = do
  w <- forShelleyBasedEraMaybeEon era
  pure $
    Opt.hsubparser $
      commandWithMetavar "stake-pool-default-vote" $
        Opt.info (QueryStakePoolDefaultVoteCmd <$> pQueryStakePoolDefaultVoteCmdArgs w) $
          Opt.progDesc "Get the stake pool default vote."
 where
  pQueryStakePoolDefaultVoteCmdArgs
    :: ConwayEraOnwards era -> Parser (QueryStakePoolDefaultVoteCmdArgs era)
  pQueryStakePoolDefaultVoteCmdArgs w =
    QueryStakePoolDefaultVoteCmdArgs w
      <$> pQueryCommons era envCli
      <*> pSPOHashSource
      <*> pMaybeOutputFile

pQueryNoArgCmdArgs
  :: forall era
   . ()
  => ConwayEraOnwards era
  -> EnvCli
  -> Parser (QueryNoArgCmdArgs era)
pQueryNoArgCmdArgs w envCli =
  QueryNoArgCmdArgs w
    <$> pQueryCommons (convert w) envCli
    <*> pMaybeOutputFile

pQueryCommons
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Parser QueryCommons
pQueryCommons w envCli =
  QueryCommons
    <$> ( LocalNodeConnectInfo
            <$> pConsensusModeParams
            <*> pNetworkId envCli
            <*> pSocketPath envCli
        )
    <*> pTarget w

pQueryEraHistoryCmd :: forall era. ShelleyBasedEra era -> EnvCli -> Maybe (Parser (QueryCmds era))
pQueryEraHistoryCmd w envCli =
  Just
    $ Opt.hsubparser
    $ commandWithMetavar "era-history"
    $ Opt.info
      ( QueryEraHistoryCmd
          <$> pQueryEraHistoryCmdArgs
      )
    $ Opt.progDesc
    $ mconcat
      [ "Obtains the era history data. The era history contains information about when era transitions happened and can "
      , "be used together with the start time to convert slot numbers to POSIX times offline (without connecting to the node). "
      , "Converting slot numbers to POSIX times is useful, for example, when calculating the cost of executing a Plutus "
      , "script. And being able to do it offline means that it can be calculated without access to a live node."
      ]
 where
  pQueryEraHistoryCmdArgs
    :: Parser (QueryEraHistoryCmdArgs era)
  pQueryEraHistoryCmdArgs =
    QueryEraHistoryCmdArgs w
      <$> pQueryCommons w envCli
      <*> pMaybeOutputFile
