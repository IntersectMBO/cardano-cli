{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Options.Query
  ( pQueryCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api as MemberStatus (MemberStatus (..))
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
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryCmds era envCli =
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
        $ subParser "tip"
        $ Opt.info (pQueryTipCmd era envCli)
        $ Opt.progDesc "Get the node's current tip (slot no, hash, block no)"
    , Just
        $ subParser "stake-pools"
        $ Opt.info (pQueryStakePoolsCmd era envCli)
        $ Opt.progDesc "Get the node's current set of stake pool ids"
    , Just
        $ subParser "stake-distribution"
        $ Opt.info (pQueryStakeDistributionCmd era envCli)
        $ Opt.progDesc "Get the node's current aggregated stake distribution"
    , Just
        $ subParser "stake-address-info"
        $ Opt.info (pQueryStakeAddressInfoCmd era envCli)
        $ Opt.progDesc $ mconcat
            [ "Get the current delegations and reward accounts filtered by stake address."
            ]
    , Just
        $ subParser "utxo"
        $ Opt.info (pQueryUTxOCmd era envCli)
        $ Opt.progDesc $ mconcat
            [ "Get a portion of the current UTxO: by tx in, by address or the whole."
            ]
    , Just
        $ subParser "ledger-state"
        $ Opt.info (pQueryLedgerStateCmd era envCli)
        $ Opt.progDesc $ mconcat
            [ "Dump the current ledger state of the node (Ledger.NewEpochState -- advanced command)"
            ]
    , Just
        $ subParser "protocol-state"
        $ Opt.info (pQueryProtocolStateCmd era envCli)
        $ Opt.progDesc $ mconcat
            [ "Dump the current protocol state of the node (Ledger.ChainDepState -- advanced command)"
            ]
    , Just
        $ subParser "stake-snapshot"
        $ Opt.info (pQueryStakeSnapshotCmd era envCli)
        $ Opt.progDesc $ mconcat
            [ "Obtain the three stake snapshots for a pool, plus the total active stake (advanced command)"
            ]
    , Just
        $ hiddenSubParser "pool-params"
        $ Opt.info (pQueryPoolStateCmd era envCli)
        $ Opt.progDesc $ mconcat
            [ "DEPRECATED.  Use query pool-state instead.  Dump the pool parameters "
            , "(Ledger.NewEpochState.esLState._delegationState._pState._pParams -- advanced command)"
            ]
    , Just
        $ subParser "leadership-schedule"
        $ Opt.info (pLeadershipScheduleCmd era envCli)
        $ Opt.progDesc "Get the slots the node is expected to mint a block in (advanced command)"
    , Just
        $ subParser "kes-period-info"
        $ Opt.info (pKesPeriodInfoCmd era envCli)
        $ Opt.progDesc "Get information about the current KES period and your node's operational certificate."
    , Just
        $ subParser "pool-state"
        $ Opt.info (pQueryPoolStateCmd era envCli)
        $ Opt.progDesc "Dump the pool state"
    , Just
        $ subParser "tx-mempool"
        $ Opt.info (pQueryTxMempoolCmd envCli)
        $ Opt.progDesc "Local Mempool info"
    , Just
        $ subParser "slot-number"
        $ Opt.info (pQuerySlotNumberCmd era envCli)
        $ Opt.progDesc "Query slot number for UTC timestamp"
    , pQueryGetConstitutionCmd era envCli
    , pQueryGetGovStateCmd era envCli
    , pQueryDRepStateCmd era envCli
    , pQueryDRepStakeDistributionCmd era envCli
    , pQueryGetCommitteeStateCmd era envCli
    ]

pQueryProtocolParametersCmd :: EnvCli -> Parser (QueryCmds era)
pQueryProtocolParametersCmd envCli =
  fmap QueryProtocolParametersCmd $
    QueryProtocolParametersCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

pQueryTipCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryTipCmd era envCli =
  fmap QueryTipCmd $
    QueryTipCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTarget era
      <*> pMaybeOutputFile

pQueryUTxOCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryUTxOCmd era envCli =
  fmap QueryUTxOCmd $
    QueryUTxOCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pQueryUTxOFilter
      <*> pNetworkId envCli
      <*> pTarget era
      <*> (optional $ pQueryOutputFormat "utxo")
      <*> pMaybeOutputFile

pQueryStakePoolsCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakePoolsCmd era envCli =
  fmap QueryStakePoolsCmd $
    QueryStakePoolsCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTarget era
      <*> (optional $ pQueryOutputFormat "stake-pools")
      <*> pMaybeOutputFile

pQueryStakeDistributionCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakeDistributionCmd era envCli =
  fmap QueryStakeDistributionCmd $
    QueryStakeDistributionCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTarget era
      <*> pMaybeOutputFile

pQueryStakeAddressInfoCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakeAddressInfoCmd era envCli =
  fmap QueryStakeAddressInfoCmd $
    QueryStakeAddressInfoCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pFilterByStakeAddress
      <*> pNetworkId envCli
      <*> pTarget era
      <*> pMaybeOutputFile

pQueryLedgerStateCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryLedgerStateCmd era envCli =
  fmap QueryLedgerStateCmd $
    QueryLedgerStateCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTarget era
      <*> pMaybeOutputFile

pQueryProtocolStateCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryProtocolStateCmd era envCli =
  fmap QueryProtocolStateCmd $
    QueryProtocolStateCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTarget era
      <*> pMaybeOutputFile

pAllStakePoolsOrSome :: Parser (AllOrOnly (Hash StakePoolKey))
pAllStakePoolsOrSome = pAll <|> pOnly
  where pAll :: Parser (AllOrOnly (Hash StakePoolKey))
        pAll = Opt.flag' All $ mconcat
          [ Opt.long "all-stake-pools"
          , Opt.help "Query for all stake pools"
          ]
        pOnly :: Parser (AllOrOnly (Hash StakePoolKey))
        pOnly = Only <$> some (pStakePoolVerificationKeyHash Nothing)

pQueryStakeSnapshotCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryStakeSnapshotCmd era envCli =
  fmap QueryStakeSnapshotCmd $
    QueryStakeSnapshotCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pAllStakePoolsOrSome
      <*> pTarget era
      <*> pMaybeOutputFile

pQueryPoolStateCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQueryPoolStateCmd era envCli =
  fmap QueryPoolStateCmd $
    QueryPoolStateCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pAllStakePoolsOrSome
      <*> pTarget era
      <*> pMaybeOutputFile

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
pLeadershipScheduleCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pLeadershipScheduleCmd era envCli =
  fmap QueryLeadershipScheduleCmd $
    QueryLeadershipScheduleCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pGenesisFile "Shelley genesis filepath"
      <*> pStakePoolVerificationKeyOrHashOrFile Nothing
      <*> pVrfSigningKeyFile
      <*> pWhichLeadershipSchedule
      <*> pTarget era
      <*> (optional $ pQueryOutputFormat "leadership-schedule")
      <*> pMaybeOutputFile

pKesPeriodInfoCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pKesPeriodInfoCmd era envCli =
  fmap QueryKesPeriodInfoCmd $
    QueryKesPeriodInfoCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pOperationalCertificateFile
      <*> pTarget era
      <*> pMaybeOutputFile

pQuerySlotNumberCmd :: CardanoEra era -> EnvCli -> Parser (QueryCmds era)
pQuerySlotNumberCmd era envCli =
  fmap QuerySlotNumberCmd $
    QuerySlotNumberCmdArgs
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTarget era
      <*> pUtcTimestamp
  where
    pUtcTimestamp =
      convertTime <$> (Opt.strArgument . mconcat)
        [ Opt.metavar "TIMESTAMP"
        , Opt.help "UTC timestamp in YYYY-MM-DDThh:mm:ssZ format"
        ]

pQueryGetConstitutionCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetConstitutionCmd era envCli = do
  w <- forEraMaybeEon era
  pure
    $ subParser "constitution"
    $ Opt.info (QueryConstitutionCmd <$> pQueryNoArgCmdArgs w era envCli)
    $ Opt.progDesc "Get the constitution"

pQueryGetGovStateCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetGovStateCmd era envCli = do
  w <- forEraMaybeEon era
  pure
    $ subParser "gov-state"
    $ Opt.info (QueryGovStateCmd <$> pQueryNoArgCmdArgs w era envCli)
    $ Opt.progDesc "Get the governance state"

-- TODO Conway: DRep State and DRep Stake Distribution parsers use DRep keys to obtain DRep credentials. This only
-- makes use of 'KeyHashObj' constructor of 'Credential kr c'. Should we also support here 'ScriptHashObj'?
-- What about 'DRep c' - this means that only 'KeyHash' constructor is in use here: should also
-- 'DRepAlwaysAbstain' and 'DRepAlwaysNoConfidence' be supported here?

pQueryDRepStateCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryDRepStateCmd era envCli = do
  w <- forEraMaybeEon era
  pure
    $ subParser "drep-state"
    $ Opt.info (QueryDRepStateCmd <$> pQueryDRepStateCmdArgs w)
    $ Opt.progDesc "Get the DRep state."
  where
    pQueryDRepStateCmdArgs :: ConwayEraOnwards era -> Parser (QueryDRepStateCmdArgs era)
    pQueryDRepStateCmdArgs w =
      QueryDRepStateCmdArgs w
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pAllOrOnlyDRepVerificationKeyOrHashOrFile
        <*> Opt.flag WithStake NoStake (mconcat
              [ Opt.long "include-stake"
              , Opt.help $ mconcat
                 [ "Also return the stake associated with each DRep. "
                 , "The result is the same as with \"drep-stake-distribution\"; "
                 , "this is a convenience option to obtain all information concerning a DRep at once. "
                 , "This is a potentially expensive query."
                 ]
              ]
            )
        <*> pTarget era
        <*> optional pOutputFile

pQueryDRepStakeDistributionCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryDRepStakeDistributionCmd era envCli = do
  w <- forEraMaybeEon era
  pure
    $ subParser "drep-stake-distribution"
    $ Opt.info (QueryDRepStakeDistributionCmd <$> pQueryDRepStakeDistributionCmdArgs w)
    $ Opt.progDesc "Get the DRep stake distribution."
  where
    pQueryDRepStakeDistributionCmdArgs :: ConwayEraOnwards era -> Parser (QueryDRepStakeDistributionCmdArgs era)
    pQueryDRepStakeDistributionCmdArgs w = QueryDRepStakeDistributionCmdArgs w
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pAllOrOnlyDRepVerificationKeyOrHashOrFile
      <*> pTarget era
      <*> optional pOutputFile

pQueryGetCommitteeStateCmd :: ()
  => CardanoEra era
  -> EnvCli
  -> Maybe (Parser (QueryCmds era))
pQueryGetCommitteeStateCmd era envCli = do
  w <- forEraMaybeEon era
  pure
    $ subParser "committee-state"
    $ Opt.info (QueryCommitteeMembersStateCmd <$> pQueryCommitteeMembersStateArgs w)
    $ Opt.progDesc "Get the committee state"
  where
    pQueryCommitteeMembersStateArgs :: ConwayEraOnwards era -> Parser (QueryCommitteeMembersStateCmdArgs era)
    pQueryCommitteeMembersStateArgs w = QueryCommitteeMembersStateCmdArgs w
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> many pCommitteeColdVerificationKeyOrHashOrFile
      <*> many pCommitteeHotKeyOrHashOrFile
      <*> many pMemberStatus
      <*> pTarget era
      <*> optional pOutputFile

    pMemberStatus :: Parser MemberStatus
    pMemberStatus =
      asum
        [ Opt.flag' MemberStatus.Active $ mconcat
            [ Opt.long "active"
            , Opt.help "Active committee members (members whose vote will count during ratification)"
            ]
        , Opt.flag' MemberStatus.Expired $ mconcat
            [ Opt.long "expired"
            , Opt.help "Expired committee members"
            ]
        , Opt.flag' MemberStatus.Unrecognized $ mconcat
            [ Opt.long "unrecognized"
            , Opt.help "Unrecognized committe members: a hot credential for an unknown cold credential"
            ]
        ]

pQueryNoArgCmdArgs :: ()
  => ConwayEraOnwards era
  -> CardanoEra era
  -> EnvCli
  -> Parser (QueryNoArgCmdArgs era)
pQueryNoArgCmdArgs w era envCli =
  QueryNoArgCmdArgs w
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> pTarget era
    <*> optional pOutputFile
