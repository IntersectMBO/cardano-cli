{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Cardano.CLI.Legacy.Options.Transaction
  ( parseTransactionCmds
  ) where

import           Cardano.Api

import           Cardano.CLI.Environment
import           Cardano.CLI.EraBased.Options.Common
import           Cardano.CLI.Legacy.Commands.Transaction
import           Cardano.CLI.Types.Common

import           Data.Foldable
import           Options.Applicative hiding (help, str)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as H
import           Prettyprinter (line, pretty)

parseTransactionCmds :: EnvCli -> Parser LegacyTransactionCmds
parseTransactionCmds envCli =
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
    TxBuild
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

  pTransactionBuildRaw :: Parser LegacyTransactionCmds
  pTransactionBuildRaw =
    TxBuildRaw
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

  pTransactionSign  :: Parser LegacyTransactionCmds
  pTransactionSign =
    TxSign
      <$> pInputTxOrTxBodyFile
      <*> many pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pTxFileOut

  pTransactionCreateWitness :: Parser LegacyTransactionCmds
  pTransactionCreateWitness =
    TxCreateWitness
      <$> pTxBodyFileIn
      <*> pWitnessSigningData
      <*> optional (pNetworkId envCli)
      <*> pOutputFile

  pTransactionAssembleTxBodyWit :: Parser LegacyTransactionCmds
  pTransactionAssembleTxBodyWit =
    TxAssembleTxBodyWitness
      <$> pTxBodyFileIn
      <*> many pWitnessFile
      <*> pOutputFile

  pTransactionSubmit :: Parser LegacyTransactionCmds
  pTransactionSubmit =
    TxSubmit
      <$> pSocketPath envCli
      <*> pConsensusModeParams
      <*> pNetworkId envCli
      <*> pTxSubmitFile

  pTransactionPolicyId :: Parser LegacyTransactionCmds
  pTransactionPolicyId = TxMintedPolicyId <$> pScript

  pTransactionCalculateMinFee :: Parser LegacyTransactionCmds
  pTransactionCalculateMinFee =
    TxCalculateMinFee
      <$> pTxBodyFileIn
      <*> pNetworkId envCli
      <*> pProtocolParamsFile
      <*> pTxInCount
      <*> pTxOutCount
      <*> pTxShelleyWitnessCount
      <*> pTxByronWitnessCount

  pTransactionCalculateMinReqUTxO :: Parser LegacyTransactionCmds
  pTransactionCalculateMinReqUTxO = TxCalculateMinRequiredUTxO
    <$> pCardanoEra envCli
    <*> pProtocolParamsFile
    <*> pTxOut

  pTxHashScriptData :: Parser LegacyTransactionCmds
  pTxHashScriptData =
    fmap TxHashScriptData
      $ pScriptDataOrFile
          "script-data"
          "The script data, in JSON syntax."
          "The script data, in the given JSON file."

  pTransactionId  :: Parser LegacyTransactionCmds
  pTransactionId = TxGetTxId <$> pInputTxOrTxBodyFile

  pTransactionView :: Parser LegacyTransactionCmds
  pTransactionView = TxView <$> pInputTxOrTxBodyFile
