{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Options.Transaction
  ( pTransactionCmds
  )
where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))
import qualified Cardano.Api.Experimental as Exp

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Parser
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Governance

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line)

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

pTransactionCmds
  :: ()
  => ShelleyBasedEra era
  -> EnvCli
  -> Maybe (Parser (TransactionCmds era))
pTransactionCmds era' envCli =
  subInfoParser
    "transaction"
    ( Opt.progDesc $
        mconcat
          [ "Transaction commands."
          ]
    )
    [ Just $
        subParser "build-raw" $
          Opt.info (pTransactionBuildRaw era') $
            Opt.progDescDoc $
              Just $
                mconcat
                  [ pretty @String "Build a transaction (low-level, inconvenient)"
                  , line
                  , line
                  , H.yellow $
                      mconcat
                        [ "Please note "
                        , H.underline "the order"
                        , " of some cmd options is crucial. If used incorrectly may produce "
                        , "undesired tx body. See nested [] notation above for details."
                        ]
                  ]
    , pTransactionBuildCmd era' envCli
    , forShelleyBasedEraInEon era' Nothing (`pTransactionBuildEstimateCmd` envCli)
    , Just $
        subParser "sign" $
          Opt.info (pTransactionSign envCli) $
            Opt.progDesc "Sign a transaction"
    , Just $
        subParser "witness" $
          Opt.info (pTransactionCreateWitness envCli) $
            Opt.progDesc "Create a transaction witness"
    , Just $
        subParser "assemble" $
          Opt.info pTransactionAssembleTxBodyWit $
            Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"
    , Just pSignWitnessBackwardCompatible
    , Just $
        subParser "submit" $
          Opt.info (pTransactionSubmit envCli) $
            Opt.progDesc $
              mconcat
                [ "Submit a transaction to the local node whose Unix domain socket "
                , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
                ]
    , Just $
        subParser "policyid" $
          Opt.info pTransactionPolicyId $
            Opt.progDesc "Calculate the PolicyId from the monetary policy script."
    , Just $
        subParser "calculate-min-fee" $
          Opt.info pTransactionCalculateMinFee $
            Opt.progDesc "Calculate the minimum fee for a transaction."
    , Just $
        subParser "calculate-min-required-utxo" $
          Opt.info (pTransactionCalculateMinReqUTxO era') $
            Opt.progDesc "Calculate the minimum required UTxO for a transaction output."
    , Just $ pCalculateMinRequiredUtxoBackwardCompatible era'
    , Just $
        subParser "hash-script-data" $
          Opt.info pTxHashScriptData $
            Opt.progDesc "Calculate the hash of script data."
    , Just $
        subParser "txid" $
          Opt.info pTransactionId $
            Opt.progDesc "Print a transaction identifier."
    ]

-- Backwards compatible parsers
calcMinValueInfo :: ShelleyBasedEra era -> ParserInfo (TransactionCmds era)
calcMinValueInfo era' =
  Opt.info (pTransactionCalculateMinReqUTxO era') $
    Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

pCalculateMinRequiredUtxoBackwardCompatible :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pCalculateMinRequiredUtxoBackwardCompatible era' =
  Opt.subparser $
    Opt.command "calculate-min-value" (calcMinValueInfo era') <> Opt.internal

assembleInfo :: ParserInfo (TransactionCmds era)
assembleInfo =
  Opt.info pTransactionAssembleTxBodyWit $
    Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

pSignWitnessBackwardCompatible :: Parser (TransactionCmds era)
pSignWitnessBackwardCompatible =
  Opt.subparser $
    Opt.command "sign-witness" assembleInfo <> Opt.internal

pScriptValidity :: Parser ScriptValidity
pScriptValidity =
  asum
    [ Opt.flag' ScriptValid $
        mconcat
          [ Opt.long "script-valid"
          , Opt.help "Assertion that the script is valid. (default)"
          ]
    , Opt.flag' ScriptInvalid $
        mconcat
          [ Opt.long "script-invalid"
          , Opt.help $
              mconcat
                [ "Assertion that the script is invalid.  "
                , "If a transaction is submitted with such a script, "
                , "the script will fail and the collateral will be taken."
                ]
          ]
    ]

pTransactionBuildCmd
  :: ShelleyBasedEra era -> EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildCmd sbe envCli = do
  era' <- forEraMaybeEon (toCardanoEra sbe)
  pure $
    subParser "build" $
      Opt.info (pCmd era') $
        Opt.progDescDoc $
          Just $
            mconcat
              [ pretty @String "Build a balanced transaction (automatically calculates fees)"
              , line
              , line
              , H.yellow $
                  mconcat
                    [ "Please note "
                    , H.underline "the order"
                    , " of some cmd options is crucial. If used incorrectly may produce "
                    , "undesired tx body. See nested [] notation above for details."
                    ]
              ]
 where
  pCmd era' = do
    fmap TransactionBuildCmd $
      TransactionBuildCmdArgs era'
        <$> ( LocalNodeConnectInfo
                <$> pConsensusModeParams
                <*> pNetworkId envCli
                <*> pSocketPath envCli
            )
        <*> optional pScriptValidity
        <*> optional pWitnessOverride
        <*> some (pTxIn sbe AutoBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pRequiredSigner
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> optional pTotalCollateral
        <*> many pTxOut
        <*> pChangeAddress
        <*> optional (pMintMultiAsset sbe AutoBalance)
        <*> optional pInvalidBefore
        <*> pInvalidHereafter sbe
        <*> many (pCertificateFile AutoBalance)
        <*> many (pWithdrawal AutoBalance)
        <*> pTxMetadataJsonSchema
        <*> many
          ( pScriptFor
              "auxiliary-script-file"
              Nothing
              "Filepath of auxiliary script(s)"
          )
        <*> many pMetadataFile
        <*> pFeatured (toCardanoEra sbe) (optional pUpdateProposalFile)
        <*> pVoteFiles sbe AutoBalance
        <*> pProposalFiles sbe AutoBalance
        <*> pMetadataChecksOnProposals
        <*> pMetadataChecksOnVotes
        <*> pTreasuryDonation sbe
        <*> pTxBuildOutputOptions

-- | Estimate the transaction fees without access to a live node.
pTransactionBuildEstimateCmd
  :: forall era. MaryEraOnwards era -> EnvCli -> Maybe (Parser (TransactionCmds era))
pTransactionBuildEstimateCmd eon' _envCli = do
  era' <- forEraMaybeEon (toCardanoEra eon')
  pure $
    subParser "build-estimate" $
      Opt.info (pCmd era') $
        Opt.progDescDoc $
          Just $
            mconcat
              [ pretty @String
                  "Build a balanced transaction without access to a live node (automatically estimates fees)"
              , line
              , line
              , H.yellow $
                  mconcat
                    [ "Please note "
                    , H.underline "the order"
                    , " of some cmd options is crucial. If used incorrectly may produce "
                    , "undesired tx body. See nested [] notation above for details."
                    ]
              ]
 where
  pCmd :: Exp.Era era -> Parser (TransactionCmds era)
  pCmd era' = do
    let sbe = convert era'
    fmap TransactionBuildEstimateCmd $
      TransactionBuildEstimateCmdArgs era'
        <$> optional pScriptValidity
        <*> pNumberOfShelleyKeyWitnesses
        <*> optional pNumberOfByronKeyWitnesses
        <*> pProtocolParamsFile
        <*> pTotalUTxOValue
        <*> some (pTxIn sbe ManualBalance)
        <*> many pReadOnlyReferenceTxIn
        <*> many pRequiredSigner
        <*> many pTxInCollateral
        <*> optional pReturnCollateral
        <*> many pTxOut
        <*> pChangeAddress
        <*> optional (pMintMultiAsset sbe ManualBalance)
        <*> optional pInvalidBefore
        <*> pInvalidHereafter sbe
        <*> many (pCertificateFile ManualBalance)
        <*> many (pWithdrawal ManualBalance)
        <*> optional pTotalCollateral
        <*> optional pReferenceScriptSize
        <*> pTxMetadataJsonSchema
        <*> many
          ( pScriptFor
              "auxiliary-script-file"
              Nothing
              "Filepath of auxiliary script(s)"
          )
        <*> many pMetadataFile
        <*> pFeatured (toCardanoEra sbe) (optional pUpdateProposalFile)
        <*> pVoteFiles sbe ManualBalance
        <*> pProposalFiles sbe ManualBalance
        <*> pCurrentTreasuryValueAndDonation sbe
        <*> pTxBodyFileOut

pChangeAddress :: Parser TxOutChangeAddress
pChangeAddress =
  fmap TxOutChangeAddress $
    Opt.option (readerFromParsecParser parseAddressAny) $
      mconcat
        [ Opt.long "change-address"
        , Opt.metavar "ADDRESS"
        , Opt.help "Address where ADA in excess of the tx fee will go to."
        ]

pTransactionBuildRaw :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pTransactionBuildRaw era' =
  fmap TransactionBuildRawCmd $
    TransactionBuildRawCmdArgs era'
      <$> optional pScriptValidity
      <*> some (pTxIn era' ManualBalance)
      <*> many pReadOnlyReferenceTxIn
      <*> many pTxInCollateral
      <*> optional pReturnCollateral
      <*> optional pTotalCollateral
      <*> many pRequiredSigner
      <*> many pTxOut
      <*> optional (pMintMultiAsset era' ManualBalance)
      <*> optional pInvalidBefore
      <*> pInvalidHereafter era'
      <*> pTxFee
      <*> many (pCertificateFile ManualBalance)
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> pFeatured era' (optional pUpdateProposalFile)
      <*> pVoteFiles era' ManualBalance
      <*> pProposalFiles era' ManualBalance
      <*> pCurrentTreasuryValueAndDonation era'
      <*> pTxBodyFileOut

pTransactionSign :: EnvCli -> Parser (TransactionCmds era)
pTransactionSign envCli =
  fmap TransactionSignCmd $
    TransactionSignCmdArgs
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pTxFileOut

pTransactionCreateWitness :: EnvCli -> Parser (TransactionCmds era)
pTransactionCreateWitness envCli =
  fmap TransactionWitnessCmd $
    TransactionWitnessCmdArgs
      <$> pTxBodyFileIn
      <*> pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pOutputFile

pTransactionAssembleTxBodyWit :: Parser (TransactionCmds era)
pTransactionAssembleTxBodyWit =
  fmap TransactionSignWitnessCmd $
    TransactionSignWitnessCmdArgs
      <$> pTxBodyFileIn
      <*> many pWitnessFile
      <*> pOutputFile

pTransactionSubmit :: EnvCli -> Parser (TransactionCmds era)
pTransactionSubmit envCli =
  fmap TransactionSubmitCmd $
    TransactionSubmitCmdArgs
      <$> ( LocalNodeConnectInfo
              <$> pConsensusModeParams
              <*> pNetworkId envCli
              <*> pSocketPath envCli
          )
      <*> pTxSubmitFile

pTransactionPolicyId :: Parser (TransactionCmds era)
pTransactionPolicyId =
  fmap TransactionPolicyIdCmd $
    TransactionPolicyIdCmdArgs
      <$> pScript

pTransactionCalculateMinFee :: Parser (TransactionCmds era)
pTransactionCalculateMinFee =
  fmap TransactionCalculateMinFeeCmd $
    TransactionCalculateMinFeeCmdArgs
      <$> pTxBodyFileIn
      <*> pProtocolParamsFile
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount
      <*> pReferenceScriptSize
      <*> (optional $ pOutputFormatJsonOrText "calculate-min-fee")
      <*> optional pOutputFile
      -- Deprecated options:
      <* optional pNetworkIdDeprecated
      <* optional pTxInCountDeprecated
      <* optional pTxOutCountDeprecated

pTransactionCalculateMinReqUTxO :: ShelleyBasedEra era -> Parser (TransactionCmds era)
pTransactionCalculateMinReqUTxO era' =
  fmap TransactionCalculateMinValueCmd $
    TransactionCalculateMinValueCmdArgs era'
      <$> pProtocolParamsFile
      <*> pTxOutShelleyBased

pTxHashScriptData :: Parser (TransactionCmds era)
pTxHashScriptData =
  fmap TransactionHashScriptDataCmd $
    TransactionHashScriptDataCmdArgs
      <$> pScriptDataOrFile
        "script-data"
        "The script data."
        "The script data file."

pTransactionId :: Parser (TransactionCmds era)
pTransactionId =
  fmap TransactionTxIdCmd $
    TransactionTxIdCmdArgs
      <$> pInputTxOrTxBodyFile
      <*> pTxIdOutputFormatJsonOrText

pMetadataChecksOnProposals :: Parser MetadataCheck
pMetadataChecksOnProposals =
  Opt.flag EnableMetadataCheck DisableMetadataCheck $
    mconcat
      [ Opt.long "disable-metadata-checks-on-proposals"
      , Opt.help "Disable automatic metadata-hash checks on proposals included in the transaction."
      ]

pMetadataChecksOnVotes :: Parser MetadataCheck
pMetadataChecksOnVotes =
  Opt.flag EnableMetadataCheck DisableMetadataCheck $
    mconcat
      [ Opt.long "disable-metadata-checks-on-votes"
      , Opt.help "Disable automatic metadata-hash checks on votes included in the transaction."
      ]
