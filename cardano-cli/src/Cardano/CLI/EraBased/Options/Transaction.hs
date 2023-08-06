{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Options.Transaction
  ( pTransactionCmds
  ) where

import           Cardano.Api hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Environment (EnvCli (..))
import           Cardano.CLI.EraBased.Commands.Transaction
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line, pretty)

pTransactionCmds :: EnvCli -> Parser TransactionCmds
pTransactionCmds envCli =
  asum
    [ subParser "build-raw"
        $ Opt.info (fmap TransactionCmdsBuildRaw pTransactionBuildRawCmd)
        $ Opt.progDescDoc $ Just $ mconcat
          [ pretty @String "Build a transaction (low-level, inconvenient)"
          , line
          , line
          , H.yellow $ mconcat
            [ "Please note the order of some cmd options is crucial. If used incorrectly may produce "
            , "undesired tx body. See nested [] notation above for details."
            ]
          ]
    , subParser "build"
        $ Opt.info (fmap TransactionCmdsBuild $ pTransactionBuildCmd)
        $ Opt.progDescDoc $ Just $ mconcat
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
        $ Opt.info (fmap TransactionCmdsSign pTransactionSignCmd)
        $ Opt.progDesc "Sign a transaction"
    , subParser "witness"
        $ Opt.info (fmap TransactionCmdsCreateWitness pTransactionCreateWitnessCmd)
        $ Opt.progDesc "Create a transaction witness"
    , subParser "assemble"
        $ Opt.info (fmap TransactionCmdsAssemble pTransactionAssembleCmd)
        $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"
    , Opt.subparser
        $ Opt.command "sign-witness" (fmap TransactionCmdsAssemble pAssembleInfo) <> Opt.internal
    , subParser "submit"
        $ Opt.info (fmap TransactionCmdsSubmit pTransactionSubmitCmd)
        $ Opt.progDesc
        $ mconcat
            [ "Submit a transaction to the local node whose Unix domain socket "
            , "is obtained from the CARDANO_NODE_SOCKET_PATH environment variable."
            ]
    , subParser "policyid"
        $ Opt.info (fmap TransactionCmdsPolicyId pTransactionPolicyIdCmd)
        $ Opt.progDesc "Calculate the PolicyId from the monetary policy script."
    , subParser "calculate-min-fee"
        $ Opt.info (fmap TransactionCmdsCalculateMinFee pTransactionCalculateMinFeeCmd)
        $ Opt.progDesc "Calculate the minimum fee for a transaction."
    , subParser "calculate-min-required-utxo"
        $ Opt.info (fmap TransactionCmdsCalculateMinRequiredUTxO pTransactionCalculateMinRequiredUTxOCmd)
        $ Opt.progDesc "Calculate the minimum required UTxO for a transaction output."
    , Opt.subparser
        $ Opt.command "calculate-min-value" (fmap TransactionCmdsCalculateMinRequiredUTxO calcMinValueInfo) <> Opt.internal
    , subParser "hash-script-data"
        $ Opt.info (fmap TransactionCmdsHashScriptData pTransactionHashScriptDataCmd)
        $ Opt.progDesc "Calculate the hash of script data."
    , subParser "txid"
        $ Opt.info (fmap TransactionCmdsTxId pTransactionTxIdCmd)
        $ Opt.progDesc "Print a transaction identifier."
    , subParser "view"
        $ Opt.info (fmap TransactionCmdsView pTransactionViewCmd)
        $ Opt.progDesc "Print a transaction."
    ]
 where
  -- Backwards compatible parsers
  calcMinValueInfo :: ParserInfo TransactionCalculateMinRequiredUTxOCmd
  calcMinValueInfo =
    Opt.info pTransactionCalculateMinRequiredUTxOCmd
      $ Opt.progDesc "DEPRECATED: Use 'calculate-min-required-utxo' instead."

  pAssembleInfo :: ParserInfo TransactionAssembleCmd
  pAssembleInfo =
    Opt.info pTransactionAssembleCmd
      $ Opt.progDesc "Assemble a tx body and witness(es) to form a transaction"

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

  pTransactionBuildCmd :: Parser TransactionBuildCmd
  pTransactionBuildCmd =
    TransactionBuildCmd
      <$> pSocketPath envCli
      <*> pCardanoEra envCli
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
      <*> optional pInvalidHereafter
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
      <*> many (pFileInDirection "constitution-file" "Filepath of the constitution.")
      <*> (OutputTxBodyOnly <$> pTxBodyFileOut <|> pCalculatePlutusScriptCost)

  pChangeAddress :: Parser TxOutChangeAddress
  pChangeAddress =
    fmap TxOutChangeAddress $ Opt.option (readerFromParsecParser parseAddressAny) $ mconcat
      [ Opt.long "change-address"
      , Opt.metavar "ADDRESS"
      , Opt.help "Address where ADA in excess of the tx fee will go to."
      ]

  pTransactionBuildRawCmd :: Parser TransactionBuildRawCmd
  pTransactionBuildRawCmd =
    TransactionBuildRawCmd
      <$> pCardanoEra envCli
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
      <*> optional pInvalidHereafter
      <*> optional pTxFee
      <*> many (pCertificateFile ManualBalance )
      <*> many (pWithdrawal ManualBalance)
      <*> pTxMetadataJsonSchema
      <*> many (pScriptFor "auxiliary-script-file" Nothing "Filepath of auxiliary script(s)")
      <*> many pMetadataFile
      <*> optional pProtocolParamsFile
      <*> optional pUpdateProposalFile
      <*> pTxBodyFileOut

  pTransactionSignCmd :: Parser TransactionSignCmd
  pTransactionSignCmd =
    TransactionSignCmd
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pTxFileOut

  pTransactionCreateWitnessCmd :: Parser TransactionCreateWitnessCmd
  pTransactionCreateWitnessCmd =
    TransactionCreateWitnessCmd
      <$> pTxBodyFileIn
      <*> pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pOutputFile

  pTransactionAssembleCmd :: Parser TransactionAssembleCmd
  pTransactionAssembleCmd =
    TransactionAssembleCmd
      <$> pTxBodyFileIn
      <*> many pWitnessFile
      <*> pOutputFile

  pTransactionSubmitCmd :: Parser TransactionSubmitCmd
  pTransactionSubmitCmd =
    TransactionSubmitCmd
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTxSubmitFile

  pTransactionPolicyIdCmd :: Parser TransactionPolicyIdCmd
  pTransactionPolicyIdCmd =
    TransactionPolicyIdCmd
      <$> pScript

  pTransactionCalculateMinFeeCmd :: Parser TransactionCalculateMinFeeCmd
  pTransactionCalculateMinFeeCmd =
    TransactionCalculateMinFeeCmd
      <$> pTxBodyFileIn
      <*> pNetworkId envCli
      <*> pProtocolParamsFile
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

  pTransactionCalculateMinRequiredUTxOCmd :: Parser TransactionCalculateMinRequiredUTxOCmd
  pTransactionCalculateMinRequiredUTxOCmd =
    TransactionCalculateMinRequiredUTxOCmd
      <$> pCardanoEra envCli
      <*> pProtocolParamsFile
      <*> pTxOut

  pTransactionHashScriptDataCmd :: Parser TransactionHashScriptDataCmd
  pTransactionHashScriptDataCmd =
    fmap TransactionHashScriptDataCmd
      $ pScriptDataOrFile
          "script-data"
          "The script data, in JSON syntax."
          "The script data, in the given JSON file."

  pTransactionTxIdCmd :: Parser TransactionTxIdCmd
  pTransactionTxIdCmd =
    TransactionTxIdCmd
      <$> pInputTxOrTxBodyFile

  pTransactionViewCmd :: Parser TransactionViewCmd
  pTransactionViewCmd =
    TransactionViewCmd
      <$> pInputTxOrTxBodyFile
