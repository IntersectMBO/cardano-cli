{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Legacy.Options
  ( -- * CLI command parser
    parseLegacyCmds

    -- * CLI command and flag types
  , module Cardano.CLI.Legacy.Commands

    -- * Field parser and renderers
  , parseTxIn

  , pLegacyCardanoEra
  , pLegacyShelleyBasedEra
  , pKeyRegistDeposit
  , pStakePoolRegistrationParserRequirements
  , pStakePoolVerificationKeyOrHashOrFile
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.Chain.Common (BlockCount (BlockCount))
import           Cardano.CLI.Environment
import qualified Cardano.CLI.EraBased.Commands.Node as Cmd
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands
import           Cardano.CLI.Legacy.Commands.Address
import           Cardano.CLI.Legacy.Commands.Genesis
import           Cardano.CLI.Legacy.Commands.Governance
import           Cardano.CLI.Legacy.Commands.Node
import qualified Cardano.CLI.Legacy.Commands.Node as Cmd
import           Cardano.CLI.Legacy.Commands.Query
import           Cardano.CLI.Legacy.Commands.StakeAddress
import           Cardano.CLI.Legacy.Commands.StakePool
import           Cardano.CLI.Legacy.Commands.TextView
import           Cardano.CLI.Legacy.Commands.Transaction
import           Cardano.CLI.Legacy.Options.Key
import           Cardano.CLI.Parser
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Data.Function
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Word (Word64)
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line, pretty)

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

--
-- Shelley CLI command parsers
--

parseLegacyCmds :: EnvCli -> Parser LegacyCmds
parseLegacyCmds envCli =
  Opt.hsubparser $ mconcat
    [ Opt.metavar "Legacy commands"
    , Opt.commandGroup "Legacy commands"
    , Opt.command "address"
        $ Opt.info (LegacyAddressCmds <$> pAddressCmds envCli)
        $ Opt.progDesc "Payment address commands"
    , Opt.command "stake-address"
        $ Opt.info (LegacyStakeAddressCmds <$> pStakeAddressCmds envCli)
        $ Opt.progDesc "Stake address commands"
    , Opt.command "key"
        $ Opt.info (LegacyKeyCmds <$> pKeyCmds)
        $ Opt.progDesc "Key utility commands"
    , Opt.command "transaction"
        $ Opt.info (LegacyTransactionCmds <$> pTransaction envCli)
        $ Opt.progDesc "Transaction commands"
    , Opt.command "node"
        $ Opt.info (LegacyNodeCmds <$> pNodeCmds)
        $ Opt.progDesc "Node operation commands"
    , Opt.command "stake-pool"
        $ Opt.info (LegacyStakePoolCmds <$> pStakePoolCmds envCli)
        $ Opt.progDesc "Stake pool commands"
    , Opt.command "query"
        $ Opt.info (LegacyQueryCmds <$> pQueryCmds envCli) . Opt.progDesc
        $ mconcat
            [ "Node query commands. Will query the local node whose Unix domain socket "
            , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
    , Opt.command "genesis"
        $ Opt.info (LegacyGenesisCmds <$> pGenesisCmds envCli)
        $ Opt.progDesc "Genesis block commands"
    , Opt.command "governance"
        $ Opt.info (LegacyGovernanceCmds <$> pGovernanceCmds envCli)
        $ Opt.progDesc "Governance commands"
    , Opt.command "text-view"
        $ Opt.info (LegacyTextViewCmds <$> pTextViewCmds) . Opt.progDesc
        $ mconcat
            [ "Commands for dealing with Shelley TextView files. "
            , "Transactions, addresses etc are stored on disk as TextView files."
            ]
    ]

pTextViewCmds :: Parser LegacyTextViewCmds
pTextViewCmds =
  asum
    [ subParser "decode-cbor"
        (Opt.info (TextViewInfo <$> pCBORInFile <*> pMaybeOutputFile)
          $ Opt.progDesc "Print a TextView file as decoded CBOR."
          )
    ]

pAddressCmds :: EnvCli -> Parser LegacyAddressCmds
pAddressCmds envCli =
   asum
     [ subParser "key-gen"
         (Opt.info pAddressKeyGen $ Opt.progDesc "Create an address key pair.")
     , subParser "key-hash"
         (Opt.info pAddressKeyHash $ Opt.progDesc "Print the hash of an address key.")
     , subParser "build"
         (Opt.info pAddressBuild $ Opt.progDesc "Build a Shelley payment address, with optional delegation to a stake address.")
     , subParser "info"
         (Opt.info pAddressInfo $ Opt.progDesc "Print information about an address.")
     ]
  where
    pAddressKeyGen :: Parser LegacyAddressCmds
    pAddressKeyGen =
      AddressKeyGen
        <$> pKeyOutputFormat
        <*> pAddressKeyType
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pAddressKeyHash :: Parser LegacyAddressCmds
    pAddressKeyHash =
      AddressKeyHash
        <$> pPaymentVerificationKeyTextOrFile
        <*> pMaybeOutputFile

    pAddressBuild :: Parser LegacyAddressCmds
    pAddressBuild = AddressBuild
      <$> pPaymentVerifier
      <*> Opt.optional pStakeIdentifier
      <*> pNetworkId envCli
      <*> pMaybeOutputFile

    pAddressInfo :: Parser LegacyAddressCmds
    pAddressInfo = AddressInfo <$> pAddress <*> pMaybeOutputFile

pStakeAddressCmds :: EnvCli -> Parser LegacyStakeAddressCmds
pStakeAddressCmds envCli =
    asum
      [ subParser "key-gen"
          $ Opt.info pStakeAddressKeyGenCmd
          $ Opt.progDesc "Create a stake address key pair"
      , subParser "build"
          $ Opt.info pStakeAddressBuildCmd
          $ Opt.progDesc "Build a stake address"
      , subParser "key-hash"
          $ Opt.info pStakeAddressKeyHashCmd
          $ Opt.progDesc "Print the hash of a stake address key."
      , subParser "registration-certificate"
          $ Opt.info pStakeAddressRegistrationCertificateCmd
          $ Opt.progDesc "Create a stake address registration certificate"
      , subParser "deregistration-certificate"
          $ Opt.info pStakeAddressDeregistrationCertificateCmd
          $ Opt.progDesc "Create a stake address deregistration certificate"
      , subParser "delegation-certificate"
          $ Opt.info pStakeAddressStakeDelegationCertificateCmd
          $ Opt.progDesc "Create a stake address pool delegation certificate"
      ]
  where
    pStakeAddressKeyGenCmd :: Parser LegacyStakeAddressCmds
    pStakeAddressKeyGenCmd =
      StakeAddressKeyGenCmd
        <$> pKeyOutputFormat
        <*> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pStakeAddressKeyHashCmd :: Parser LegacyStakeAddressCmds
    pStakeAddressKeyHashCmd =
      StakeAddressKeyHashCmd
        <$> pStakeVerificationKeyOrFile Nothing
        <*> pMaybeOutputFile

    pStakeAddressBuildCmd :: Parser LegacyStakeAddressCmds
    pStakeAddressBuildCmd =
      StakeAddressBuildCmd
        <$> pStakeVerifier
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pStakeAddressRegistrationCertificateCmd :: Parser LegacyStakeAddressCmds
    pStakeAddressRegistrationCertificateCmd =
      StakeAddressRegistrationCertificateCmd
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressDeregistrationCertificateCmd :: Parser LegacyStakeAddressCmds
    pStakeAddressDeregistrationCertificateCmd =
      StakeAddressDeregistrationCertificateCmd
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> optional pKeyRegistDeposit
        <*> pOutputFile

    pStakeAddressStakeDelegationCertificateCmd :: Parser LegacyStakeAddressCmds
    pStakeAddressStakeDelegationCertificateCmd =
      StakeAddressDelegationCertificateCmd
        <$> pAnyShelleyBasedEra envCli
        <*> pStakeIdentifier
        <*> pStakePoolVerificationKeyOrHashOrFile Nothing
        <*> pOutputFile

pTransaction :: EnvCli -> Parser LegacyTransactionCmds
pTransaction envCli =
  asum
    [ subParser "build-raw"
        $ Opt.info pTransactionBuildRaw $ Opt.progDescDoc $ Just $ mconcat
          [ pretty @String "Build a transaction (low-level, inconvenient)"
          , line
          , line
          , H.yellow $ mconcat
            [ "Please note the order of some cmd options is crucial. If used incorrectly may produce "
            , "undesired tx body. See nested [] notation above for details."
            ]
          ]
    , subParser "build"
        $ Opt.info pTransactionBuild $ Opt.progDescDoc $ Just $ mconcat
          [ pretty @String "Build a balanced transaction (automatically calculates fees)"
          , line
          , line
          , H.yellow $ mconcat
            [ "Please note "
            , H.underline "the order"
            , " of some cmd options is crucial. If used incorrectly may produce "
            , "undesired tx body. See nested [] notation above for details."
            ]
          ]
    , subParser "sign"
        (Opt.info pTransactionSign $ Opt.progDesc "Sign a transaction")
    , subParser "witness"
        (Opt.info pTransactionCreateWitness $ Opt.progDesc "Create a transaction witness")
    , subParser "assemble"
        (Opt.info pTransactionAssembleTxBodyWit
          $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction")
    , pSignWitnessBackwardCompatible
    , subParser "submit"
        (Opt.info pTransactionSubmit . Opt.progDesc $
           mconcat
             [ "Submit a transaction to the local node whose Unix domain socket "
             , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
             ]
          )
    , subParser "policyid"
        (Opt.info pTransactionPolicyId $ Opt.progDesc "Calculate the PolicyId from the monetary policy script.")
    , subParser "calculate-min-fee"
        (Opt.info pTransactionCalculateMinFee $ Opt.progDesc "Calculate the minimum fee for a transaction.")
    , subParser "calculate-min-required-utxo"
        (Opt.info pTransactionCalculateMinReqUTxO $ Opt.progDesc "Calculate the minimum required UTxO for a transaction output.")
    , pCalculateMinRequiredUtxoBackwardCompatible
    , subParser "hash-script-data"
        (Opt.info pTxHashScriptData $ Opt.progDesc "Calculate the hash of script data.")
    , subParser "txid"
        (Opt.info pTransactionId $ Opt.progDesc "Print a transaction identifier.")
    , subParser "view" $
        Opt.info pTransactionView $ Opt.progDesc "Print a transaction."
    ]
 where
  -- Backwards compatible parsers
  calcMinValueInfo :: ParserInfo LegacyTransactionCmds
  calcMinValueInfo =
    Opt.info pTransactionCalculateMinReqUTxO
      $ Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

  pCalculateMinRequiredUtxoBackwardCompatible :: Parser LegacyTransactionCmds
  pCalculateMinRequiredUtxoBackwardCompatible =
    Opt.subparser
      $ Opt.command "calculate-min-value" calcMinValueInfo <> Opt.internal

  assembleInfo :: ParserInfo LegacyTransactionCmds
  assembleInfo =
    Opt.info pTransactionAssembleTxBodyWit
      $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

  pSignWitnessBackwardCompatible :: Parser LegacyTransactionCmds
  pSignWitnessBackwardCompatible =
    Opt.subparser
      $ Opt.command "sign-witness" assembleInfo <> Opt.internal

  pScriptValidity :: Parser ScriptValidity
  pScriptValidity = asum
    [ Opt.flag' ScriptValid $ mconcat
      [ Opt.long "script-valid"
      , Opt.help "Assertion that the script is valid. (default)"
      ]
    , Opt.flag' ScriptInvalid $ mconcat
      [ Opt.long "script-invalid"
      , Opt.help $ mconcat
        [ "Assertion that the script is invalid.  "
        , "If a transaction is submitted with such a script, "
        , "the script will fail and the collateral will be taken."
        ]
      ]
    ]

  pTransactionBuild :: Parser LegacyTransactionCmds
  pTransactionBuild =
    TransactionBuildCmd
      <$> pSocketPath envCli
      <*> pLegacyShelleyBasedEra envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> optional pScriptValidity
      <*> optional pWitnessOverride
      <*> some (pTxIn AutoBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pRequiredSigner
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pTxOut
      <*> pChangeAddress
      <*> optional (pMintMultiAsset AutoBalance)
      <*> optional pInvalidBefore
      <*> optional pLegacyInvalidHereafter
      <*> many (pCertificateFile AutoBalance)
      <*> many (pWithdrawal AutoBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor
                  "auxiliary-script-file"
                  Nothing
                  "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pUpdateProposalFile
      <*> many (pFileInDirection "vote-file" "Filepath of the vote.")
      <*> many (pFileInDirection "proposal-file" "Filepath of the proposal.")
      <*> (OutputTxBodyOnly <$> pTxBodyFileOut <|> pCalculatePlutusScriptCost)

  pChangeAddress :: Parser TxOutChangeAddress
  pChangeAddress =
    fmap TxOutChangeAddress $ Opt.option (readerFromParsecParser parseAddressAny) $ mconcat
      [ Opt.long "change-address"
      , Opt.metavar "ADDRESS"
      , Opt.help "Address where ADA in excess of the tx fee will go to."
      ]

  pTransactionBuildRaw :: Parser LegacyTransactionCmds
  pTransactionBuildRaw =
    TransactionBuildRawCmd
      <$> pLegacyCardanoEra envCli
      <*> optional pScriptValidity
      <*> some (pTxIn ManualBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pRequiredSigner
      <*> many pTxOut
      <*> optional (pMintMultiAsset ManualBalance)
      <*> optional pInvalidBefore
      <*> optional pLegacyInvalidHereafter
      <*> optional pTxFee
      <*> many (pCertificateFile ManualBalance )
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> optional pUpdateProposalFile
      <*> pTxBodyFileOut

  pTransactionSign  :: Parser LegacyTransactionCmds
  pTransactionSign =
    TransactionSignCmd
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pTxFileOut

  pTransactionCreateWitness :: Parser LegacyTransactionCmds
  pTransactionCreateWitness =
    TransactionWitnessCmd
      <$> pTxBodyFileIn
      <*> pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pOutputFile

  pTransactionAssembleTxBodyWit :: Parser LegacyTransactionCmds
  pTransactionAssembleTxBodyWit =
    TransactionSignWitnessCmd
      <$> pTxBodyFileIn
      <*> many pWitnessFile
      <*> pOutputFile

  pTransactionSubmit :: Parser LegacyTransactionCmds
  pTransactionSubmit =
    TransactionSubmitCmd
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTxSubmitFile

  pTransactionPolicyId :: Parser LegacyTransactionCmds
  pTransactionPolicyId =
    TransactionPolicyIdCmd
      <$> pScript

  pTransactionCalculateMinFee :: Parser LegacyTransactionCmds
  pTransactionCalculateMinFee =
    TransactionCalculateMinFeeCmd
      <$> pTxBodyFileIn
      <*> pNetworkId envCli
      <*> pProtocolParamsFile
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

  pTransactionCalculateMinReqUTxO :: Parser LegacyTransactionCmds
  pTransactionCalculateMinReqUTxO =
    TransactionCalculateMinValueCmd
      <$> pAnyShelleyBasedEra envCli
      <*> pProtocolParamsFile
      <*> pTxOutShelleyBased

  pTxHashScriptData :: Parser LegacyTransactionCmds
  pTxHashScriptData =
    fmap TransactionHashScriptDataCmd
      $ pScriptDataOrFile
          "script-data"
          "The script data, in JSON syntax."
          "The script data, in the given JSON file."

  pTransactionId  :: Parser LegacyTransactionCmds
  pTransactionId =
    TransactionTxIdCmd
      <$> pInputTxOrTxBodyFile

  pTransactionView :: Parser LegacyTransactionCmds
  pTransactionView =
    TransactionViewCmd
      <$> pTxViewOutputFormat
      <*> pMaybeOutputFile
      <*> pInputTxOrTxBodyFile

pNodeCmds :: Parser LegacyNodeCmds
pNodeCmds =
  asum
    [ subParser "key-gen" . Opt.info pKeyGenOperator . Opt.progDesc $ mconcat
      [ "Create a key pair for a node operator's offline "
      , "key and a new certificate issue counter"
      ]
    , subParser "key-gen-KES" . Opt.info pKeyGenKES . Opt.progDesc $ mconcat
      [ "Create a key pair for a node KES operational key"
      ]
    , subParser "key-gen-VRF" . Opt.info pKeyGenVRF . Opt.progDesc $ mconcat
      [ "Create a key pair for a node VRF operational key"
      ]
    , subParser "key-hash-VRF". Opt.info pKeyHashVRF . Opt.progDesc $ mconcat
      [ "Print hash of a node's operational VRF key."
      ]
    , subParser "new-counter" . Opt.info pNewCounter . Opt.progDesc $ mconcat
      [ "Create a new certificate issue counter"
      ]
    , subParser "issue-op-cert" . Opt.info pIssueOpCert . Opt.progDesc $ mconcat
      [ "Issue a node operational certificate"
      ]
    ]
  where
    pKeyGenOperator :: Parser LegacyNodeCmds
    pKeyGenOperator =
      fmap Cmd.LegacyNodeKeyGenColdCmd $
        Cmd.NodeKeyGenColdCmdArgs
          <$> pKeyOutputFormat
          <*> pColdVerificationKeyFile
          <*> pColdSigningKeyFile
          <*> pOperatorCertIssueCounterFile

    pKeyGenKES :: Parser LegacyNodeCmds
    pKeyGenKES =
      fmap Cmd.LegacyNodeKeyGenKESCmd $
        Cmd.NodeKeyGenKESCmdArgs
          <$> pKeyOutputFormat
          <*> pVerificationKeyFileOut
          <*> pSigningKeyFileOut

    pKeyGenVRF :: Parser LegacyNodeCmds
    pKeyGenVRF =
      fmap Cmd.LegacyNodeKeyGenVRFCmd $
        Cmd.NodeKeyGenVRFCmdArgs
          <$> pKeyOutputFormat
          <*> pVerificationKeyFileOut
          <*> pSigningKeyFileOut

    pKeyHashVRF :: Parser LegacyNodeCmds
    pKeyHashVRF =
      fmap Cmd.LegacyNodeKeyHashVRFCmd $
        Cmd.NodeKeyHashVRFCmdArgs
          <$> pVerificationKeyOrFileIn AsVrfKey
          <*> pMaybeOutputFile

    pNewCounter :: Parser LegacyNodeCmds
    pNewCounter =
      fmap Cmd.LegacyNodeNewCounterCmd $
        Cmd.NodeNewCounterCmdArgs
          <$> pColdVerificationKeyOrFile Nothing
          <*> pCounterValue
          <*> pOperatorCertIssueCounterFile

    pCounterValue :: Parser Word
    pCounterValue =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "counter-value"
        , Opt.metavar "INT"
        , Opt.help "The next certificate issue counter value to use."
        ]

    pIssueOpCert :: Parser LegacyNodeCmds
    pIssueOpCert =
      fmap Cmd.LegacyNodeIssueOpCertCmd $
        Cmd.NodeIssueOpCertCmdArgs
          <$> pKesVerificationKeyOrFile
          <*> pColdSigningKeyFile
          <*> pOperatorCertIssueCounterFile
          <*> pKesPeriod
          <*> pOutputFile

pStakePoolCmds :: EnvCli -> Parser LegacyStakePoolCmds
pStakePoolCmds  envCli =
  asum
    [ subParser "registration-certificate"
        $ Opt.info (pStakePoolRegistrationCertificiateCmd envCli)
        $ Opt.progDesc "Create a stake pool registration certificate"
    , subParser "deregistration-certificate"
        $ Opt.info (pStakePoolDeregistrationCertificateCmd envCli)
        $ Opt.progDesc "Create a stake pool deregistration certificate"
    , subParser "id"
        $ Opt.info pStakePoolId
        $ Opt.progDesc "Build pool id from the offline key"
    , subParser "metadata-hash"
        $ Opt.info pStakePoolMetadataHashCmd
        $ Opt.progDesc "Print the hash of pool metadata."
    ]
  where
    pStakePoolId :: Parser LegacyStakePoolCmds
    pStakePoolId =
      StakePoolIdCmd
        <$> pStakePoolVerificationKeyOrFile Nothing
        <*> pPoolIdOutputFormat
        <*> pMaybeOutputFile

    pStakePoolMetadataHashCmd :: Parser LegacyStakePoolCmds
    pStakePoolMetadataHashCmd =
      StakePoolMetadataHashCmd
        <$> pPoolMetadataFile
        <*> pMaybeOutputFile

pQueryCmds :: EnvCli -> Parser LegacyQueryCmds
pQueryCmds envCli =
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
      fmap QueryProtocolParametersCmd $
        LegacyQueryProtocolParametersCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> pMaybeOutputFile

    pQueryConstitutionHash :: Parser LegacyQueryCmds
    pQueryConstitutionHash =
      fmap QueryConstitutionHashCmd $
        LegacyQueryConstitutionHashCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> pMaybeOutputFile

    pQueryTip :: Parser LegacyQueryCmds
    pQueryTip =
      fmap QueryTipCmd $
        LegacyQueryTipCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> pMaybeOutputFile

    pQueryUTxO :: Parser LegacyQueryCmds
    pQueryUTxO =
      fmap QueryUTxOCmd $
        LegacyQueryUTxOCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pQueryUTxOFilter
          <*> pNetworkId envCli
          <*> pMaybeOutputFile

    pQueryStakePools :: Parser LegacyQueryCmds
    pQueryStakePools =
      fmap QueryStakePoolsCmd $
        LegacyQueryStakePoolsCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> pMaybeOutputFile

    pQueryStakeDistribution :: Parser LegacyQueryCmds
    pQueryStakeDistribution =
      fmap QueryStakeDistributionCmd $
        LegacyQueryStakeDistributionCmdArgs
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryStakeAddressInfo :: Parser LegacyQueryCmds
    pQueryStakeAddressInfo =
      fmap QueryStakeAddressInfoCmd $
        LegacyQueryStakeAddressInfoCmdArgs
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pFilterByStakeAddress
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryLedgerState :: Parser LegacyQueryCmds
    pQueryLedgerState =
      fmap QueryLedgerStateCmd $
        LegacyQueryLedgerStateCmdArgs
        <$> pSocketPath envCli
        <*> pConsensusModeParams
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pQueryProtocolState :: Parser LegacyQueryCmds
    pQueryProtocolState =
      fmap QueryProtocolStateCmd $
        LegacyQueryProtocolStateCmdArgs
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

    pQueryStakeSnapshot :: Parser LegacyQueryCmds
    pQueryStakeSnapshot =
      fmap QueryStakeSnapshotCmd $
        LegacyQueryStakeSnapshotCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> pAllStakePoolsOrOnly
          <*> pMaybeOutputFile

    pQueryPoolState :: Parser LegacyQueryCmds
    pQueryPoolState =
      fmap QueryPoolStateCmd $
        LegacyQueryPoolStateCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> many (pStakePoolVerificationKeyHash Nothing)

    pQueryTxMempool :: Parser LegacyQueryCmds
    pQueryTxMempool =
      fmap QueryTxMempoolCmd $
        LegacyQueryTxMempoolCmdArgs
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
      fmap QueryLeadershipScheduleCmd $
        LegacyQueryLeadershipScheduleCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> pGenesisFile "Shelley genesis filepath"
          <*> pStakePoolVerificationKeyOrHashOrFile Nothing
          <*> pVrfSigningKeyFile
          <*> pWhichLeadershipSchedule
          <*> pMaybeOutputFile

    pKesPeriodInfo :: Parser LegacyQueryCmds
    pKesPeriodInfo =
      fmap QueryKesPeriodInfoCmd $
        LegacyQueryKesPeriodInfoCmdArgs
          <$> pSocketPath envCli
          <*> pConsensusModeParams
          <*> pNetworkId envCli
          <*> pOperationalCertificateFile
          <*> pMaybeOutputFile

    pQuerySlotNumber :: Parser LegacyQueryCmds
    pQuerySlotNumber =
      fmap QuerySlotNumberCmd $
        LegacyQuerySlotNumberCmdArgs
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


-- TODO: Conway era - move to Cardano.CLI.Conway.Parsers
pGovernanceCmds :: EnvCli -> Parser LegacyGovernanceCmds
pGovernanceCmds envCli =
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
      GovernanceCreateMirCertificateStakeAddressesCmd
        <$> pAnyShelleyToBabbageEra envCli
        <*> pMIRPot
        <*> some pStakeAddress
        <*> some pRewardAmt
        <*> pOutputFile

    pLegacyMIRTransferToTreasury :: Parser LegacyGovernanceCmds
    pLegacyMIRTransferToTreasury =
      GovernanceCreateMirCertificateTransferToTreasuryCmd
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile

    pLegacyMIRTransferToReserves :: Parser LegacyGovernanceCmds
    pLegacyMIRTransferToReserves =
      GovernanceCreateMirCertificateTransferToReservesCmd
        <$> pAnyShelleyToBabbageEra envCli
        <*> pTransferAmt
        <*> pOutputFile

    pGovernanceGenesisKeyDelegationCertificate :: Parser LegacyGovernanceCmds
    pGovernanceGenesisKeyDelegationCertificate =
      GovernanceGenesisKeyDelegationCertificate
        <$> pAnyShelleyToBabbageEra envCli
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

pGenesisCmds :: EnvCli -> Parser LegacyGenesisCmds
pGenesisCmds envCli =
  asum
    [ subParser "key-gen-genesis"
        $ Opt.info pGenesisKeyGen
        $ Opt.progDesc "Create a Shelley genesis key pair"
    , subParser "key-gen-delegate"
        $ Opt.info pGenesisDelegateKeyGen
        $ Opt.progDesc "Create a Shelley genesis delegate key pair"
    , subParser "key-gen-utxo"
        $ Opt.info pGenesisUTxOKeyGen
        $ Opt.progDesc "Create a Shelley genesis UTxO key pair"
    , subParser "key-hash"
        $ Opt.info pGenesisKeyHash
        $ Opt.progDesc "Print the identifier (hash) of a public key"
    , subParser "get-ver-key"
        $ Opt.info pGenesisVerKey
        $ Opt.progDesc "Derive the verification key from a signing key"
    , subParser "initial-addr"
        $ Opt.info pGenesisAddr
        $ Opt.progDesc "Get the address for an initial UTxO based on the verification key"
    , subParser "initial-txin"
        $ Opt.info pGenesisTxIn
        $ Opt.progDesc "Get the TxIn for an initial UTxO based on the verification key"
    , subParser "create-cardano"
        $ Opt.info pGenesisCreateCardano
        $ Opt.progDesc
        $ mconcat
            [ "Create a Byron and Shelley genesis file from a genesis "
            , "template and genesis/delegation/spending keys."
            ]
    , subParser "create"
        $ Opt.info pGenesisCreate
        $ Opt.progDesc
        $ mconcat
            [ "Create a Shelley genesis file from a genesis "
            , "template and genesis/delegation/spending keys."
            ]
    , subParser "create-staked"
        $ Opt.info pGenesisCreateStaked
        $ Opt.progDesc
        $ mconcat
            [ "Create a staked Shelley genesis file from a genesis "
            , "template and genesis/delegation/spending keys."
            ]
    , subParser "hash"
        $ Opt.info pGenesisHash
        $ Opt.progDesc "Compute the hash of a genesis file"
    ]
  where
    pGenesisKeyGen :: Parser LegacyGenesisCmds
    pGenesisKeyGen =
      GenesisKeyGenGenesis
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pGenesisDelegateKeyGen :: Parser LegacyGenesisCmds
    pGenesisDelegateKeyGen =
      GenesisKeyGenDelegate
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut
        <*> pOperatorCertIssueCounterFile

    pGenesisUTxOKeyGen :: Parser LegacyGenesisCmds
    pGenesisUTxOKeyGen =
      GenesisKeyGenUTxO
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileOut

    pGenesisKeyHash :: Parser LegacyGenesisCmds
    pGenesisKeyHash =
      GenesisCmdKeyHash
        <$> pVerificationKeyFileIn

    pGenesisVerKey :: Parser LegacyGenesisCmds
    pGenesisVerKey =
      GenesisVerKey
        <$> pVerificationKeyFileOut
        <*> pSigningKeyFileIn

    pGenesisAddr :: Parser LegacyGenesisCmds
    pGenesisAddr =
      GenesisAddr
        <$> pVerificationKeyFileIn
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pGenesisTxIn :: Parser LegacyGenesisCmds
    pGenesisTxIn =
      GenesisTxIn
        <$> pVerificationKeyFileIn
        <*> pNetworkId envCli
        <*> pMaybeOutputFile

    pGenesisCreateCardano :: Parser LegacyGenesisCmds
    pGenesisCreateCardano =
      GenesisCreateCardano
        <$> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> (BlockCount <$> pSecurityParam)
        <*> pSlotLength
        <*> pSlotCoefficient
        <*> pNetworkId envCli
        <*> parseFilePath
              "byron-template"
              "JSON file with genesis defaults for each byron."
        <*> parseFilePath
              "shelley-template"
              "JSON file with genesis defaults for each shelley."
        <*> parseFilePath
              "alonzo-template"
              "JSON file with genesis defaults for alonzo."
        <*> parseFilePath
              "conway-template"
              "JSON file with genesis defaults for conway."
        <*> pNodeConfigTemplate

    pGenesisCreate :: Parser LegacyGenesisCmds
    pGenesisCreate =
      GenesisCreate
        <$> pKeyOutputFormat
        <*> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> pNetworkId envCli

    pGenesisCreateStaked :: Parser LegacyGenesisCmds
    pGenesisCreateStaked =
      GenesisCreateStaked
        <$> pKeyOutputFormat
        <*> pGenesisDir
        <*> pGenesisNumGenesisKeys
        <*> pGenesisNumUTxOKeys
        <*> pGenesisNumPools
        <*> pGenesisNumStDelegs
        <*> pMaybeSystemStart
        <*> pInitialSupplyNonDelegated
        <*> pInitialSupplyDelegated
        <*> pNetworkId envCli
        <*> pBulkPoolCredFiles
        <*> pBulkPoolsPerFile
        <*> pStuffedUtxoCount
        <*> Opt.optional pRelayJsonFp

    pGenesisHash :: Parser LegacyGenesisCmds
    pGenesisHash =
      GenesisHashFile <$> pGenesisFile "The genesis file."

    pGenesisDir :: Parser GenesisDir
    pGenesisDir =
      fmap GenesisDir $ Opt.strOption $ mconcat
        [ Opt.long "genesis-dir"
        , Opt.metavar "DIR"
        , Opt.help "The genesis directory containing the genesis template and required genesis/delegation/spending keys."
        ]

    pMaybeSystemStart :: Parser (Maybe SystemStart)
    pMaybeSystemStart =
      Opt.optional $ fmap (SystemStart . convertTime) $ Opt.strOption $ mconcat
        [ Opt.long "start-time"
        , Opt.metavar "UTC-TIME"
        , Opt.help "The genesis start time in YYYY-MM-DDThh:mm:ssZ format. If unspecified, will be the current time +30 seconds."
        ]

    pGenesisNumGenesisKeys :: Parser Word
    pGenesisNumGenesisKeys =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-genesis-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of genesis keys to make [default is 3]."
        , Opt.value 3
        ]

    pNodeConfigTemplate :: Parser (Maybe FilePath)
    pNodeConfigTemplate = optional $ parseFilePath "node-config-template" "the node config template"

    pGenesisNumUTxOKeys :: Parser Word
    pGenesisNumUTxOKeys =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-utxo-keys"
        , Opt.metavar "INT"
        , Opt.help "The number of UTxO keys to make [default is 0]."
        , Opt.value 0
        ]

    pGenesisNumPools :: Parser Word
    pGenesisNumPools =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-pools"
        , Opt.metavar "INT"
        , Opt.help "The number of stake pool credential sets to make [default is 0]."
        , Opt.value 0
        ]

    pGenesisNumStDelegs :: Parser Word
    pGenesisNumStDelegs =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "gen-stake-delegs"
        , Opt.metavar "INT"
        , Opt.help "The number of stake delegator credential sets to make [default is 0]."
        , Opt.value 0
        ]

    pStuffedUtxoCount :: Parser Word
    pStuffedUtxoCount =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "num-stuffed-utxo"
        , Opt.metavar "INT"
        , Opt.help "The number of fake UTxO entries to generate [default is 0]."
        , Opt.value 0
        ]

    pRelayJsonFp :: Parser FilePath
    pRelayJsonFp =
      Opt.strOption $ mconcat
        [ Opt.long "relay-specification-file"
        , Opt.metavar "FILE"
        , Opt.help "JSON file specified the relays of each stake pool."
        , Opt.completer (Opt.bashCompleter "file")
        ]

    pInitialSupplyNonDelegated :: Parser (Maybe Lovelace)
    pInitialSupplyNonDelegated =
      Opt.optional $ fmap Lovelace $ Opt.option Opt.auto $ mconcat
        [ Opt.long "supply"
        , Opt.metavar "LOVELACE"
        , Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial, non-delegating stake holders."
        ]

    pInitialSupplyDelegated :: Parser Lovelace
    pInitialSupplyDelegated =
      fmap (Lovelace . fromMaybe 0) $ Opt.optional $ Opt.option Opt.auto $ mconcat
        [ Opt.long "supply-delegated"
        , Opt.metavar "LOVELACE"
        , Opt.help "The initial coin supply in Lovelace which will be evenly distributed across initial, delegating stake holders."
        , Opt.value 0
        ]

    pSecurityParam :: Parser Word64
    pSecurityParam =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "security-param"
        , Opt.metavar "INT"
        , Opt.help "Security parameter for genesis file [default is 108]."
        , Opt.value 108
        ]

    pSlotLength :: Parser Word
    pSlotLength =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "slot-length"
        , Opt.metavar "INT"
        , Opt.help "slot length (ms) parameter for genesis file [default is 1000]."
        , Opt.value 1000
        ]


    pSlotCoefficient :: Parser Rational
    pSlotCoefficient =
      Opt.option readRationalUnitInterval $ mconcat
        [ Opt.long "slot-coefficient"
        , Opt.metavar "RATIONAL"
        , Opt.help "Slot Coefficient for genesis file [default is .05]."
        , Opt.value 0.05
        ]

    pBulkPoolCredFiles :: Parser Word
    pBulkPoolCredFiles =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "bulk-pool-cred-files"
        , Opt.metavar "INT"
        , Opt.help "Generate bulk pool credential files [default is 0]."
        , Opt.value 0
        ]

    pBulkPoolsPerFile :: Parser Word
    pBulkPoolsPerFile =
      Opt.option Opt.auto $ mconcat
        [ Opt.long "bulk-pools-per-file"
        , Opt.metavar "INT"
        , Opt.help "Each bulk pool to contain this many pool credential sets [default is 0]."
        , Opt.value 0
        ]

pStakePoolRegistrationCertificiateCmd :: EnvCli -> Parser LegacyStakePoolCmds
pStakePoolRegistrationCertificiateCmd envCli =
  StakePoolRegistrationCertificateCmd
    <$> pAnyShelleyBasedEra envCli
    <*> pStakePoolVerificationKeyOrFile Nothing
    <*> pVrfVerificationKeyOrFile
    <*> pPoolPledge
    <*> pPoolCost
    <*> pPoolMargin
    <*> pRewardAcctVerificationKeyOrFile
    <*> some pPoolOwnerVerificationKeyOrFile
    <*> many pPoolRelay
    <*> pStakePoolMetadataReference
    <*> pNetworkId envCli
    <*> pOutputFile

pStakePoolDeregistrationCertificateCmd :: EnvCli -> Parser LegacyStakePoolCmds
pStakePoolDeregistrationCertificateCmd envCli =
  StakePoolDeregistrationCertificateCmd
    <$> pAnyShelleyBasedEra envCli
    <*> pStakePoolVerificationKeyOrFile Nothing
    <*> pEpochNo "The epoch number."
    <*> pOutputFile

pLegacyCardanoEra :: EnvCli -> Parser AnyCardanoEra
pLegacyCardanoEra envCli =
  asum $ mconcat
    [ [ Opt.flag' (AnyCardanoEra ByronEra) $ mconcat
        [ Opt.long "byron-era"
        , Opt.help "Specify the Byron era"
        ]
      , Opt.flag' (AnyCardanoEra ShelleyEra) $ mconcat
        [ Opt.long "shelley-era"
        , Opt.help "Specify the Shelley era"
        ]
      , Opt.flag' (AnyCardanoEra AllegraEra) $ mconcat
        [ Opt.long "allegra-era"
        , Opt.help "Specify the Allegra era"
        ]
      , Opt.flag' (AnyCardanoEra MaryEra) $ mconcat
        [ Opt.long "mary-era"
        , Opt.help "Specify the Mary era"
        ]
      , Opt.flag' (AnyCardanoEra AlonzoEra) $ mconcat
        [ Opt.long "alonzo-era"
        , Opt.help "Specify the Alonzo era"
        ]
      , Opt.flag' (AnyCardanoEra BabbageEra) $ mconcat
        [ Opt.long "babbage-era"
        , Opt.help "Specify the Babbage era (default)"
        ]
      ]
    , maybeToList $ pure <$> envCliAnyCardanoEra envCli
    -- TODO is this default needed anymore?
    , pure $ pure defaultCardanoEra
  ]
    where
      defaultCardanoEra = defaultShelleyBasedEra & \(EraInEon era) ->
        let cera = toCardanoEra era
         in cardanoEraConstraints cera (AnyCardanoEra cera)

pLegacyShelleyBasedEra :: EnvCli -> Parser (EraInEon ShelleyBasedEra)
pLegacyShelleyBasedEra envCli =
  asum $ mconcat
    [ [ Opt.flag' (EraInEon ShelleyBasedEraShelley) $ mconcat
        [ Opt.long "shelley-era"
        , Opt.help "Specify the Shelley era"
        ]
      , Opt.flag' (EraInEon ShelleyBasedEraAllegra) $ mconcat
        [ Opt.long "allegra-era"
        , Opt.help "Specify the Allegra era"
        ]
      , Opt.flag' (EraInEon ShelleyBasedEraMary) $ mconcat
        [ Opt.long "mary-era"
        , Opt.help "Specify the Mary era"
        ]
      , Opt.flag' (EraInEon ShelleyBasedEraAlonzo) $ mconcat
        [ Opt.long "alonzo-era"
        , Opt.help "Specify the Alonzo era"
        ]
      , Opt.flag' (EraInEon ShelleyBasedEraBabbage) $ mconcat
        [ Opt.long "babbage-era"
        , Opt.help "Specify the Babbage era (default)"
        ]
      ]
    , maybeToList $ pure <$> envCliAnyShelleyBasedEra envCli
    , pure $ pure defaultShelleyBasedEra
  ]
