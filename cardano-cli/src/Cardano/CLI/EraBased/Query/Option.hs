{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Cardano.Api.Experimental

import Cardano.CLI.Environment (EnvCli (..))
import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Query.Command
import Cardano.CLI.Option.Flag
import Cardano.CLI.Parser
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Key

import Data.Foldable
import Data.Function
import GHC.Exts (IsList (..))
import Options.Applicative hiding (help, str)
import Options.Applicative qualified as Opt
import Vary (Vary)

pQueryCmdsTopLevel :: IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryCmdsTopLevel envCli =
  let parsers =
        [ pProtocolParams envCli
        , pTip envCli
        , pStakePools envCli
        , pStakeDistribution envCli
        , pStakeAddressInfo envCli
        , pQueryEraHistoryCmd envCli
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

pTip :: IsEra era => EnvCli -> Parser (QueryCmds era)
pTip envCli =
  Opt.hsubparser $
    commandWithMetavar "tip" $
      Opt.info (pQueryTipCmd envCli) $
        Opt.progDesc "Get the node's current tip (slot no, hash, block no)"

pStakePools :: IsEra era => EnvCli -> Parser (QueryCmds era)
pStakePools envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-pools" $
      Opt.info (pQueryStakePoolsCmd envCli) $
        Opt.progDesc "Get the node's current set of stake pool ids"

pStakeDistribution :: IsEra era => EnvCli -> Parser (QueryCmds era)
pStakeDistribution envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-distribution" $
      Opt.info (pQueryStakeDistributionCmd envCli) $
        Opt.progDesc "Get the node's current aggregated stake distribution"

pStakeAddressInfo :: IsEra era => EnvCli -> Parser (QueryCmds era)
pStakeAddressInfo envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-address-info" $
      Opt.info (pQueryStakeAddressInfoCmd envCli) $
        Opt.progDesc $
          mconcat
            [ "Get the current delegations and reward accounts filtered by stake address."
            ]

pUTxO :: IsEra era => EnvCli -> Parser (QueryCmds era)
pUTxO envCli =
  Opt.hsubparser $
    commandWithMetavar "utxo" $
      Opt.info (pQueryUTxOCmd envCli) $
        Opt.progDesc $
          mconcat
            [ "Get a portion of the current UTxO: by tx in, by address or the whole."
            ]

pLedgerState :: IsEra era => EnvCli -> Parser (QueryCmds era)
pLedgerState envCli =
  Opt.hsubparser $
    commandWithMetavar "ledger-state" $
      Opt.info (pQueryLedgerStateCmd envCli) $
        Opt.progDesc $
          mconcat
            [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
            ]

pProtocolState :: IsEra era => EnvCli -> Parser (QueryCmds era)
pProtocolState envCli =
  Opt.hsubparser $
    commandWithMetavar "protocol-state" $
      Opt.info (pQueryProtocolStateCmd envCli) $
        Opt.progDesc $
          mconcat
            [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
            ]

pStakeSnapshot :: IsEra era => EnvCli -> Parser (QueryCmds era)
pStakeSnapshot envCli =
  Opt.hsubparser $
    commandWithMetavar "stake-snapshot" $
      Opt.info (pQueryStakeSnapshotCmd envCli) $
        Opt.progDesc $
          mconcat
            [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
            ]

pPoolParams :: IsEra era => EnvCli -> Parser (QueryCmds era)
pPoolParams envCli =
  Opt.hsubparser $
    mconcat
      [ Opt.hidden
      , commandWithMetavar "pool-params" $
          Opt.info (pQueryPoolStateCmd envCli) $
            Opt.progDesc $
              mconcat
                [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
                , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
                ]
      ]

pLeadershipSchedule :: IsEra era => EnvCli -> Parser (QueryCmds era)
pLeadershipSchedule envCli =
  Opt.hsubparser $
    commandWithMetavar "leadership-schedule" $
      Opt.info (pLeadershipScheduleCmd envCli) $
        Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"

pKesPeriodInfo :: IsEra era => EnvCli -> Parser (QueryCmds era)
pKesPeriodInfo envCli =
  Opt.hsubparser $
    commandWithMetavar "kes-period-info" $
      Opt.info (pKesPeriodInfoCmd envCli) $
        Opt.progDesc "Get information about the current KES period and your node's operational certificate."

pPoolState :: IsEra era => EnvCli -> Parser (QueryCmds era)
pPoolState envCli =
  Opt.hsubparser $
    commandWithMetavar "pool-state" $
      Opt.info (pQueryPoolStateCmd envCli) $
        Opt.progDesc "Dump the pool state"

pTxMempool :: EnvCli -> Parser (QueryCmds era)
pTxMempool envCli =
  Opt.hsubparser $
    commandWithMetavar "tx-mempool" $
      Opt.info (pQueryTxMempoolCmd envCli) $
        Opt.progDesc "Local Mempool info"

pSlotNumber :: IsEra era => EnvCli -> Parser (QueryCmds era)
pSlotNumber envCli =
  Opt.hsubparser $
    commandWithMetavar "slot-number" $
      Opt.info (pQuerySlotNumberCmd envCli) $
        Opt.progDesc "Query slot number for UTC timestamp"

pQueryLedgerPeerSnapshot :: IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryLedgerPeerSnapshot envCli =
  Opt.hsubparser $
    commandWithMetavar "ledger-peer-snapshot" $
      Opt.info (pQueryLedgerPeerSnapshotCmd envCli) $
        Opt.progDesc $
          mconcat
            [ "Dump the current snapshot of big ledger peers. "
            , "These are the largest pools that cumulatively hold "
            , "90% of total stake."
            ]

-- \^ TODO use bigLedgerPeerQuota from Ouroboros.Network.PeerSelection.LedgerPeers.Utils
-- which must be re-exposed thru cardano-api

pQueryCmds
  :: IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryCmds envCli =
  subInfoParser
    "query"
    ( Opt.progDesc $
        mconcat
          [ "Node query commands. Will query the local node whose Unix domain socket is "
          , "obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
          ]
    )
    [ pQueryGetCommitteeStateCmd envCli
    , pQueryGetConstitutionCmd envCli
    , pQueryDRepStateCmd envCli
    , pQueryDRepStakeDistributionCmd envCli
    , Just $ pQueryEraHistoryCmd envCli
    , pQueryFuturePParamsCmd envCli
    , pQueryGetGovStateCmd envCli
    , Just
        . Opt.hsubparser
        . commandWithMetavar "kes-period-info"
        . Opt.info (pKesPeriodInfoCmd envCli)
        $ Opt.progDesc "Get information about the current KES period and your node's operational certificate."
    , Just
        . Opt.hsubparser
        . commandWithMetavar "leadership-schedule"
        . Opt.info (pLeadershipScheduleCmd envCli)
        $ Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"
    , Just
        . Opt.hsubparser
        . commandWithMetavar "ledger-peer-snapshot"
        . Opt.info (pQueryLedgerPeerSnapshotCmd envCli)
        . Opt.progDesc
        $ mconcat
          [ "Dump the current snapshot of ledger peers."
          , "These are the largest pools that cumulatively hold "
          , "90% of total stake."
          ]
    , -- \^ TODO use bigLedgerPeerQuota from Ouroboros.Network.PeerSelection.LedgerPeers.Utils
      -- which must be re-exposed thru cardano-api
      Just
        . Opt.hsubparser
        . commandWithMetavar "ledger-state"
        . Opt.info (pQueryLedgerStateCmd envCli)
        . Opt.progDesc
        $ mconcat
          [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
          ]
    , Just
        . Opt.hsubparser
        $ mconcat
          [ Opt.hidden
          , commandWithMetavar "pool-params"
              . Opt.info (pQueryPoolStateCmd envCli)
              . Opt.progDesc
              $ mconcat
                [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
                , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
                ]
          ]
    , Just
        . Opt.hsubparser
        . commandWithMetavar "pool-state"
        . Opt.info (pQueryPoolStateCmd envCli)
        $ Opt.progDesc "Dump the pool state"
    , pQueryProposalsCmd envCli
    , Just $
        Opt.hsubparser $
          commandWithMetavar "protocol-parameters" $
            Opt.info (pQueryProtocolParametersCmd envCli) $
              Opt.progDesc "Get the node's current protocol parameters"
    , Just
        . Opt.hsubparser
        . commandWithMetavar "protocol-state"
        . Opt.info (pQueryProtocolStateCmd envCli)
        . Opt.progDesc
        $ mconcat
          [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
          ]
    , pQueryGetRatifyStateCmd envCli
    , Just
        . Opt.hsubparser
        . commandWithMetavar "ref-script-size"
        . Opt.info (pQueryRefScriptSizeCmd envCli)
        $ Opt.progDesc "Calculate the reference input scripts size in bytes for provided transaction inputs."
    , Just
        $ Opt.hsubparser
          . commandWithMetavar "slot-number"
          . Opt.info (pQuerySlotNumberCmd envCli)
        $ Opt.progDesc "Query slot number for UTC timestamp"
    , pQuerySPOStakeDistributionCmd envCli
    , Just
        . Opt.hsubparser
        . commandWithMetavar "stake-address-info"
        . Opt.info (pQueryStakeAddressInfoCmd envCli)
        . Opt.progDesc
        $ mconcat
          [ "Get the current delegations and reward accounts filtered by stake address."
          ]
    , Just
        . Opt.hsubparser
        . commandWithMetavar "stake-distribution"
        . Opt.info (pQueryStakeDistributionCmd envCli)
        $ Opt.progDesc "Get the node's current aggregated stake distribution"
    , Just
        . Opt.hsubparser
        . commandWithMetavar "stake-pools"
        . Opt.info (pQueryStakePoolsCmd envCli)
        $ Opt.progDesc "Get the node's current set of stake pool ids"
    , pQueryStakePoolDefaultVote envCli
    , Just
        . Opt.hsubparser
        . commandWithMetavar "stake-snapshot"
        . Opt.info (pQueryStakeSnapshotCmd envCli)
        . Opt.progDesc
        $ mconcat
          [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
          ]
    , Just
        . Opt.hsubparser
        . commandWithMetavar "tip"
        . Opt.info (pQueryTipCmd envCli)
        $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)"
    , pQueryTreasuryValueCmd envCli
    , Just
        . Opt.hsubparser
        . commandWithMetavar "tx-mempool"
        . Opt.info (pQueryTxMempoolCmd envCli)
        $ Opt.progDesc "Local Mempool info"
    , Just
        . Opt.hsubparser
        . commandWithMetavar "utxo"
        . Opt.info (pQueryUTxOCmd envCli)
        . Opt.progDesc
        $ mconcat
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
      <*> pFormatQueryOutputFlags
        "protocol-parameters"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryTipCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryTipCmd envCli =
  fmap QueryTipCmd $
    QueryTipCmdArgs
      <$> pQueryCommons @era envCli
      <*> pFormatQueryOutputFlags
        "tip"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryUTxOCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryUTxOCmd envCli =
  fmap QueryUTxOCmd $
    QueryUTxOCmdArgs
      <$> pQueryCommons @era envCli
      <*> pQueryUTxOFilter
      <*> pFormatQueryOutputFlags
        "utxo"
        [ flagFormatCborBin
        , flagFormatCborHex
        , flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryStakePoolsCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryStakePoolsCmd envCli =
  fmap QueryStakePoolsCmd $
    QueryStakePoolsCmdArgs
      <$> pQueryCommons @era envCli
      <*> pFormatQueryOutputFlags
        "stake-pools"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryStakeDistributionCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryStakeDistributionCmd envCli =
  fmap QueryStakeDistributionCmd $
    QueryStakeDistributionCmdArgs
      <$> pQueryCommons @era envCli
      <*> pFormatQueryOutputFlags
        "stake-distribution"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryStakeAddressInfoCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryStakeAddressInfoCmd envCli =
  fmap QueryStakeAddressInfoCmd $
    QueryStakeAddressInfoCmdArgs
      <$> pQueryCommons @era envCli
      <*> pFilterByStakeAddress
      <*> pFormatQueryOutputFlags
        "stake-address-info"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryLedgerStateCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryLedgerStateCmd envCli =
  fmap QueryLedgerStateCmd $
    QueryLedgerStateCmdArgs
      <$> pQueryCommons @era envCli
      <*> pFormatQueryOutputFlags
        "ledger-state"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryLedgerPeerSnapshotCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryLedgerPeerSnapshotCmd envCli =
  fmap QueryLedgerPeerSnapshotCmd $
    QueryLedgerPeerSnapshotCmdArgs
      <$> pQueryCommons @era envCli
      <*> pFormatQueryOutputFlags
        "ledger-peer-snapshot"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryProtocolStateCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryProtocolStateCmd envCli =
  fmap QueryProtocolStateCmd $
    QueryProtocolStateCmdArgs
      <$> pQueryCommons @era envCli
      <*> pFormatQueryOutputFlags
        "protocol-state"
        [ flagFormatCborBin
        , flagFormatCborHex
        , flagFormatJson & setDefault
        , flagFormatYaml
        ]
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

pQueryStakeSnapshotCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryStakeSnapshotCmd envCli =
  fmap QueryStakeSnapshotCmd $
    QueryStakeSnapshotCmdArgs
      <$> pQueryCommons @era envCli
      <*> pAllStakePoolsOrSome
      <*> pFormatQueryOutputFlags
        "stake-snapshot"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryPoolStateCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryPoolStateCmd envCli =
  fmap QueryPoolStateCmd $
    QueryPoolStateCmdArgs
      <$> pQueryCommons @era envCli
      <*> pAllStakePoolsOrSome
      <*> pFormatQueryOutputFlags
        "pool-state"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
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
      <*> pFormatQueryOutputFlags
        "tx-mempool"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile
 where
  pTxMempoolQuery :: Parser TxMempoolQuery
  pTxMempoolQuery =
    asum
      [ Opt.hsubparser
          . commandWithMetavar "info"
          . Opt.info (pure TxMempoolQueryInfo)
          $ Opt.progDesc "Ask the node about the current mempool's capacity and sizes"
      , Opt.hsubparser
          . commandWithMetavar "next-tx"
          . Opt.info (pure TxMempoolQueryNextTx)
          $ Opt.progDesc "Requests the next transaction from the mempool's current list"
      , Opt.hsubparser
          . commandWithMetavar "tx-exists"
          . Opt.info (TxMempoolQueryTxExists <$> argument (readerFromParsecParser parseTxId) (metavar "TX_ID"))
          $ Opt.progDesc "Query if a particular transaction exists in the mempool"
      ]

pLeadershipScheduleCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pLeadershipScheduleCmd envCli =
  fmap QueryLeadershipScheduleCmd $
    QueryLeadershipScheduleCmdArgs
      <$> pQueryCommons @era envCli
      <*> pGenesisFile "Shelley genesis filepath"
      <*> pStakePoolVerificationKeyOrHashOrFile Nothing
      <*> pVrfSigningKeyFile
      <*> pWhichLeadershipSchedule
      <*> pFormatQueryOutputFlags
        "leadership-schedule"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pKesPeriodInfoCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pKesPeriodInfoCmd envCli =
  fmap QueryKesPeriodInfoCmd $
    QueryKesPeriodInfoCmdArgs
      <$> pQueryCommons @era envCli
      <*> pOperationalCertificateFile
      <*> pFormatQueryOutputFlags
        "kes-period-info"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQuerySlotNumberCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQuerySlotNumberCmd envCli =
  fmap QuerySlotNumberCmd $
    QuerySlotNumberCmdArgs
      <$> pQueryCommons @era envCli
      <*> pUtcTimestamp
 where
  pUtcTimestamp =
    convertTime
      <$> (Opt.strArgument . mconcat)
        [ Opt.metavar "TIMESTAMP"
        , Opt.help "UTC timestamp in YYYY-MM-DDThh:mm:ssZ format"
        ]

pQueryRefScriptSizeCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryRefScriptSizeCmd envCli =
  fmap QueryRefScriptSizeCmd $
    QueryRefScriptSizeCmdArgs
      <$> pQueryCommons @era envCli
      <*> (fromList <$> some pByTxIn)
      <*> pFormatQueryOutputFlags
        "reference-script-size"
        [ flagFormatJson & setDefault
        , flagFormatText
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile
 where
  pByTxIn :: Parser TxIn
  pByTxIn =
    Opt.option (readerFromParsecParser parseTxIn) $
      mconcat
        [ Opt.long "tx-in"
        , Opt.metavar "TX_IN"
        , Opt.help "Transaction input (TxId#TxIx)."
        ]

pQueryGetConstitutionCmd
  :: IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetConstitutionCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "constitution"
    . Opt.info (QueryConstitutionCmd <$> pQueryNoArgCmdArgs envCli "constitution")
    $ Opt.progDesc "Get the constitution"

pQueryGetGovStateCmd
  :: IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetGovStateCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "gov-state"
    . Opt.info (QueryGovStateCmd <$> pQueryNoArgCmdArgs envCli "gov-state")
    $ Opt.progDesc "Get the governance state"

pQueryGetRatifyStateCmd
  :: IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetRatifyStateCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "ratify-state"
    . Opt.info (QueryRatifyStateCmd <$> pQueryNoArgCmdArgs envCli "ratify-state")
    $ Opt.progDesc "Get the ratification state"

pQueryFuturePParamsCmd
  :: IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryFuturePParamsCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "future-pparams"
    . Opt.info (QueryFuturePParamsCmd <$> pQueryNoArgCmdArgs envCli "future-pparams")
    $ Opt.progDesc "Get the protocol parameters that will apply at the next epoch"

-- TODO Conway: DRep State and DRep Stake Distribution parsers use DRep keys to obtain DRep credentials. This only
-- makes use of 'KeyHashObj' constructor of 'Credential kr c'. Should we also support here 'ScriptHashObj'?
-- What about 'DRep c' - this means that only 'KeyHash' constructor is in use here: should also
-- 'DRepAlwaysAbstain' and 'DRepAlwaysNoConfidence' be supported here?

pQueryDRepStateCmd
  :: forall era
   . IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryDRepStateCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "drep-state"
    . Opt.info (QueryDRepStateCmd <$> pQueryDRepStateCmdArgs useEra)
    $ Opt.progDesc "Get the DRep state."
 where
  pQueryDRepStateCmdArgs :: Era era -> Parser (QueryDRepStateCmdArgs era)
  pQueryDRepStateCmdArgs w =
    QueryDRepStateCmdArgs (convert w)
      <$> pQueryCommons @era envCli
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
      <*> pFormatQueryOutputFlags
        "drep-state"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryDRepStakeDistributionCmd
  :: forall era
   . IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryDRepStakeDistributionCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "drep-stake-distribution"
    . Opt.info (QueryDRepStakeDistributionCmd <$> pQueryDRepStakeDistributionCmdArgs useEra)
    $ Opt.progDesc "Get the DRep stake distribution."
 where
  pQueryDRepStakeDistributionCmdArgs
    :: Era era -> Parser (QueryDRepStakeDistributionCmdArgs era)
  pQueryDRepStakeDistributionCmdArgs w =
    QueryDRepStakeDistributionCmdArgs (convert w)
      <$> pQueryCommons @era envCli
      <*> pAllOrOnlyDRepHashSource
      <*> pFormatQueryOutputFlags
        "drep-stake-distribution"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryProposalsCmd
  :: forall era
   . IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryProposalsCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "proposals"
    . Opt.info (QueryProposalsCmd <$> (pQueryProposalsCmdArgs))
    . Opt.progDesc
    $ mconcat
      [ "Get the governance proposals that are eligible for ratification. "
      , "Proposals submitted during the current epoch are excluded, as they cannot be ratified until the next epoch. "
      ]
 where
  pQueryProposalsCmdArgs :: Parser (QueryProposalsCmdArgs era)
  pQueryProposalsCmdArgs =
    QueryProposalsCmdArgs (convert useEra)
      <$> pQueryCommons @era envCli
      <*> pAllOrOnlyGovActionIds
      <*> pFormatQueryOutputFlags
        "proposals"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> optional pOutputFile

pQuerySPOStakeDistributionCmd
  :: forall era
   . IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQuerySPOStakeDistributionCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "spo-stake-distribution"
    . Opt.info (QuerySPOStakeDistributionCmd <$> pQuerySPOStakeDistributionCmdArgs useEra)
    $ Opt.progDesc "Get the SPO stake distribution."
 where
  pQuerySPOStakeDistributionCmdArgs
    :: Era era -> Parser (QuerySPOStakeDistributionCmdArgs era)
  pQuerySPOStakeDistributionCmdArgs w =
    QuerySPOStakeDistributionCmdArgs (convert w)
      <$> pQueryCommons @era envCli
      <*> pAllOrOnlySPOHashSource
      <*> pFormatQueryOutputFlags
        "spo-stake-distribution"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryGetCommitteeStateCmd
  :: forall era
   . IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetCommitteeStateCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "committee-state"
    . Opt.info (QueryCommitteeMembersStateCmd <$> pQueryCommitteeMembersStateArgs useEra)
    $ Opt.progDesc "Get the committee state"
 where
  pQueryCommitteeMembersStateArgs
    :: Era era -> Parser (QueryCommitteeMembersStateCmdArgs era)
  pQueryCommitteeMembersStateArgs w =
    QueryCommitteeMembersStateCmdArgs (convert w)
      <$> pQueryCommons @era envCli
      <*> many pCommitteeColdVerificationKeyOrHashOrFileOrScriptHash
      <*> many pCommitteeHotKeyOrHashOrFileOrScriptHash
      <*> many pMemberStatus
      <*> pFormatQueryOutputFlags
        "committee-state"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
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
  :: forall era
   . IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryTreasuryValueCmd envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "treasury"
    . Opt.info (QueryTreasuryValueCmd <$> pQueryTreasuryValueArgs useEra)
    $ Opt.progDesc "Get the treasury value"
 where
  pQueryTreasuryValueArgs
    :: Era era -> Parser (QueryTreasuryValueCmdArgs era)
  pQueryTreasuryValueArgs w =
    QueryTreasuryValueCmdArgs (convert w)
      <$> pQueryCommons @era envCli
      <*> pMaybeOutputFile

pQueryStakePoolDefaultVote
  :: forall era
   . IsEra era
  => EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryStakePoolDefaultVote envCli = do
  pure
    . Opt.hsubparser
    . commandWithMetavar "stake-pool-default-vote"
    . Opt.info (QueryStakePoolDefaultVoteCmd <$> pQueryStakePoolDefaultVoteCmdArgs)
    $ Opt.progDesc "Get the stake pool default vote."
 where
  pQueryStakePoolDefaultVoteCmdArgs
    :: Parser (QueryStakePoolDefaultVoteCmdArgs era)
  pQueryStakePoolDefaultVoteCmdArgs =
    QueryStakePoolDefaultVoteCmdArgs (convert useEra)
      <$> pQueryCommons @era envCli
      <*> pSPOHashSource
      <*> pFormatQueryOutputFlags
        "stake-pool-default-vote"
        [ flagFormatJson & setDefault
        , flagFormatYaml
        ]
      <*> pMaybeOutputFile

pQueryNoArgCmdArgs
  :: forall era
   . ()
  => IsEra era
  => EnvCli
  -> String
  -> Parser (QueryNoArgCmdArgs era)
pQueryNoArgCmdArgs envCli name =
  QueryNoArgCmdArgs (convert useEra)
    <$> pQueryCommons @era envCli
    <*> pFormatQueryOutputFlags
      name
      [ flagFormatJson & setDefault
      , flagFormatYaml
      ]
    <*> pMaybeOutputFile

pQueryCommons
  :: forall era
   . IsEra era
  => EnvCli
  -> Parser QueryCommons
pQueryCommons envCli =
  QueryCommons
    <$> ( LocalNodeConnectInfo
            <$> pConsensusModeParams
            <*> pNetworkId envCli
            <*> pSocketPath envCli
        )
    <*> pTarget @era

pQueryEraHistoryCmd :: forall era. IsEra era => EnvCli -> Parser (QueryCmds era)
pQueryEraHistoryCmd envCli =
  Opt.hsubparser
    . commandWithMetavar
      "era-history"
    . Opt.info
      ( QueryEraHistoryCmd
          <$> pQueryEraHistoryCmdArgs
      )
    . Opt.progDesc
    $ mconcat
      [ "Obtains the era history data. The era history contains information about when era transitions happened and can "
      , "be used together with the start time to convert slot numbers to POSIX times offline (without connecting to the node). "
      , "Converting slot numbers to POSIX times is useful, for example, when calculating the cost of executing a Plutus "
      , "script. And being able to do it offline means that it can be calculated without access to a live node."
      ]
 where
  pQueryEraHistoryCmdArgs
    :: Parser QueryEraHistoryCmdArgs
  pQueryEraHistoryCmdArgs =
    QueryEraHistoryCmdArgs
      <$> pQueryCommons @era envCli
      <*> pMaybeOutputFile

pFormatQueryOutputFlags
  :: String
  -> [Flag (Vary fs)]
  -> Parser (Vary fs)
pFormatQueryOutputFlags content =
  pFormatFlags $ content <> " query output"
