{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Compatible.Transaction
  ( CompatibleTransactionCmds (..)
  , CompatibleTransactionError (..)
  , pAllCompatibleTransactionCommands
  , renderCompatibleTransactionCmd
  , runCompatibleTransactionCmd
  )
where

import Cardano.Api
import Cardano.Api.Compatible
import Cardano.Api.Ledger hiding (TxIn, VotingProcedures)
import Cardano.Api.Shelley hiding (VotingProcedures)

import Cardano.CLI.Environment
import Cardano.CLI.EraBased.Options.Common hiding (pRefScriptFp, pTxOutDatum)
import Cardano.CLI.EraBased.Run.Transaction
import Cardano.CLI.EraBased.Script.Certificate.Read
import Cardano.CLI.EraBased.Script.Certificate.Types
import Cardano.CLI.EraBased.Script.Proposal.Types
import Cardano.CLI.EraBased.Script.Types
import Cardano.CLI.EraBased.Script.Vote.Types
  ( CliVoteScriptRequirements
  , VoteScriptWitness (..)
  )
import Cardano.CLI.Parser
import Cardano.CLI.Read
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.BootstrapWitnessError
import Cardano.CLI.Types.Errors.TxCmdError
import Cardano.CLI.Types.Governance
import Cardano.CLI.Types.TxFeature

import RIO

import Data.Bifunctor (first)
import Data.Foldable hiding (toList)
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import GHC.Exts (toList)
import Options.Applicative
import Options.Applicative qualified as Opt

pAllCompatibleTransactionCommands
  :: EnvCli -> ShelleyBasedEra era -> Parser (CompatibleTransactionCmds era)
pAllCompatibleTransactionCommands envCli sbe =
  let allCommannds =
        asum
          [ pCompatibleSignedTransactionCommand envCli sbe
          ]
   in subParser "transaction" $
        Opt.info allCommannds $
          Opt.progDesc "Transaction commands."

pCompatibleSignedTransactionCommand
  :: EnvCli -> ShelleyBasedEra era -> Parser (CompatibleTransactionCmds era)
pCompatibleSignedTransactionCommand envCli sbe =
  subParser "signed-transaction" $
    Opt.info (pCompatibleSignedTransaction envCli sbe) $
      Opt.progDesc "Create a simple signed transaction."

pCompatibleSignedTransaction
  :: EnvCli -> ShelleyBasedEra era -> Parser (CompatibleTransactionCmds era)
pCompatibleSignedTransaction env sbe =
  CreateCompatibleSignedTransaction sbe
    <$> many pTxInOnly
    <*> many (pTxOutEraAware sbe)
    <*> pFeatured (toCardanoEra sbe) (optional pUpdateProposalFile)
    <*> pFeatured (toCardanoEra sbe) (many (pProposalFile ManualBalance))
    <*> pVoteFiles sbe ManualBalance
    <*> many pWitnessSigningData
    <*> optional (pNetworkId env)
    <*> pTxFee
    <*> many (pCertificateFile ManualBalance)
    <*> pOutputFile

pTxInOnly :: Parser TxIn
pTxInOnly =
  Opt.option
    (readerFromParsecParser parseTxIn)
    ( Opt.long "tx-in"
        <> Opt.metavar "TX-IN"
        <> Opt.help "TxId#TxIx"
    )

-- This parser renders the appropriate parsers depending on what
-- functionality is available per era.
pTxOutEraAware :: ShelleyBasedEra era -> Parser TxOutAnyEra
pTxOutEraAware sbe =
  Opt.option
    (readerFromParsecParser parseTxOutAnyEra)
    ( Opt.long "tx-out"
        <> Opt.metavar "ADDRESS VALUE"
        -- TODO alonzo: Update the help text to describe the new syntax as well.
        <> Opt.help
          "The transaction output as ADDRESS VALUE where ADDRESS is \
          \the Bech32-encoded address followed by the value in \
          \the multi-asset syntax (including simply Lovelace)."
    )
    <*> pTxOutDatum sbe
    <*> pRefScriptFp sbe

pTxOutDatum :: ShelleyBasedEra era -> Parser TxOutDatumAnyEra
pTxOutDatum =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const $ pure TxOutDatumByNone)
    ( \case
        AlonzoEraOnwardsAlonzo ->
          pAlonzoDatumFunctionality <|> pure TxOutDatumByNone
        AlonzoEraOnwardsBabbage ->
          pBabbageDatumFunctionality <|> pure TxOutDatumByNone
        AlonzoEraOnwardsConway -> pConwayDatumFunctionality <|> pure TxOutDatumByNone
    )
 where
  pAlonzoDatumFunctionality =
    asum
      [ pTxOutDatumByHashOnly
      , pTxOutDatumByHashOf
      , pTxOutDatumByValue
      ]
  pBabbageDatumFunctionality =
    asum
      [ pAlonzoDatumFunctionality
      , pTxOutInlineDatumByValue
      ]

  pConwayDatumFunctionality = pBabbageDatumFunctionality

  pTxOutDatumByHashOnly =
    fmap TxOutDatumByHashOnly $
      Opt.option (readerFromParsecParser $ parseHash (AsHash AsScriptData)) $
        mconcat
          [ Opt.long "tx-out-datum-hash"
          , Opt.metavar "HASH"
          , Opt.help $
              mconcat
                [ "The script datum hash for this tx output, as "
                , "the raw datum hash (in hex)."
                ]
          ]

  pTxOutDatumByHashOf =
    TxOutDatumByHashOf
      <$> pScriptDataOrFile
        "tx-out-datum-hash"
        "The script datum hash for this tx output, by hashing the script datum given here."
        "The script datum hash for this tx output, by hashing the script datum in the file."

  pTxOutDatumByValue =
    TxOutDatumByValue
      <$> pScriptDataOrFile
        "tx-out-datum-embed"
        "The script datum to embed in the tx for this output, given here."
        "The script datum to embed in the tx for this output, in the given file."

  pTxOutInlineDatumByValue =
    TxOutInlineDatumByValue
      <$> pScriptDataOrFile
        "tx-out-inline-datum"
        "The script datum to embed in the tx output as an inline datum, given here."
        "The script datum to embed in the tx output as an inline datum, in the given file."

pRefScriptFp :: ShelleyBasedEra era -> Parser ReferenceScriptAnyEra
pRefScriptFp =
  caseShelleyToBabbageOrConwayEraOnwards
    (const $ pure ReferenceScriptAnyEraNone)
    ( const $
        ReferenceScriptAnyEra
          <$> parseFilePath "tx-out-reference-script-file" "Reference script input file."
            <|> pure ReferenceScriptAnyEraNone
    )

-- TODO: After QA confirmms that the new compatibility commands meet their needs
-- we can remove all remaining legacy commands. We can also remove/move the exising
-- byron era commands under the new compatiblilty commands.
data CompatibleTransactionCmds era
  = CreateCompatibleSignedTransaction
      (ShelleyBasedEra era)
      [TxIn]
      [TxOutAnyEra]
      !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
      !(Maybe (Featured ConwayEraOnwards era [(ProposalFile In, Maybe CliProposalScriptRequirements)]))
      ![(VoteFile In, Maybe CliVoteScriptRequirements)]
      [WitnessSigningData]
      -- ^ Signing keys
      (Maybe NetworkId)
      !Coin
      -- ^ Tx fee
      ![(CertificateFile, Maybe CliCertificateScriptRequirements)]
      -- ^ stake registering certs
      !(File () Out)

renderCompatibleTransactionCmd :: CompatibleTransactionCmds era -> Text
renderCompatibleTransactionCmd _ = ""

data CompatibleTransactionError
  = CompatibleTxCmdError !TxCmdError
  | forall err. Error err => CompatibleFileError (FileError err)
  | CompatibleProposalError !ProposalError

instance Show CompatibleTransactionError where
  show = show . prettyError

instance Error CompatibleTransactionError where
  prettyError = \case
    CompatibleTxCmdError e -> renderTxCmdError e
    CompatibleFileError e -> prettyError e
    CompatibleProposalError e -> pshow e

runCompatibleTransactionCmd
  :: forall era
   . HasCallStack
  => CompatibleTransactionCmds era
  -> RIO () ()
runCompatibleTransactionCmd
  ( CreateCompatibleSignedTransaction
      sbe
      ins
      outs
      mUpdateProposal
      mProposalProcedure
      mVotes
      witnesses
      mNetworkId
      fee
      certificates
      outputFp
    ) =
    shelleyBasedEraConstraints sbe $ do
      sks <- mapM (fromEitherIOCli . readWitnessSigningData) witnesses

      allOuts <- fromEitherIOCli . runExceptT $ mapM (toTxOutInAnyEra sbe) outs

      certFilesAndMaybeScriptWits <-
        fromEitherIOCli $
          runExceptT $
            readCertificateScriptWitnesses sbe certificates

      certsAndMaybeScriptWits <-
        liftIO $
          sequenceA
            [ fmap (,cswScriptWitness <$> mSwit) $
                fromEitherIOCli $
                  readFileTextEnvelope AsCertificate $
                    File certFile
            | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
            ]

      (protocolUpdates, votes) :: (AnyProtocolUpdate era, AnyVote era) <-
        caseShelleyToBabbageOrConwayEraOnwards
          ( const $ do
              case mUpdateProposal of
                Nothing -> return (NoPParamsUpdate sbe, NoVotes)
                Just p -> do
                  pparamUpdate <- fromEitherIOCli $ runExceptT $ readUpdateProposalFile p
                  return (pparamUpdate, NoVotes)
          )
          ( \w ->
              case mProposalProcedure of
                Nothing -> return (NoPParamsUpdate sbe, NoVotes)
                Just prop -> do
                  pparamUpdate <- fromEitherIOCli $ runExceptT $ readProposalProcedureFile prop
                  votesAndWits <- fromEitherIOCli (readVotingProceduresFiles w mVotes)
                  votingProcedures <-
                    fromEitherCli $ mkTxVotingProcedures [(v, vswScriptWitness <$> mSwit) | (v, mSwit) <- votesAndWits]
                  return (pparamUpdate, VotingProcedures w votingProcedures)
          )
          sbe

      let certsRefInputs =
            [ refInput
            | (_, Just sWit) <- certsAndMaybeScriptWits
            , refInput <- maybeToList $ getScriptWitnessReferenceInput sWit
            ]

          votesRefInputs =
            [ refInput
            | VotingProcedures _ (TxVotingProcedures _ (BuildTxWith voteMap)) <- [votes]
            , sWit <- Map.elems voteMap
            , refInput <- maybeToList $ getScriptWitnessReferenceInput sWit
            ]

          proposalsRefInputs =
            [ refInput
            | ProposalProcedures _ (TxProposalProcedures proposalMap) <- [protocolUpdates]
            , BuildTxWith (Just sWit) <- map snd $ toList proposalMap
            , refInput <- maybeToList $ getScriptWitnessReferenceInput sWit
            ]

      validatedRefInputs <-
        fromEitherCli . first CompatibleTxCmdError . validateTxInsReference $
          certsRefInputs <> votesRefInputs <> proposalsRefInputs
      let txCerts = mkTxCertificates sbe certsAndMaybeScriptWits

      -- this body is only for witnesses
      apiTxBody <-
        fromEitherCli $
          createTransactionBody sbe $
            defaultTxBodyContent sbe
              & setTxIns (map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) ins)
              & setTxOuts allOuts
              & setTxFee (TxFeeExplicit sbe fee)
              & setTxCertificates txCerts
              & setTxInsReference validatedRefInputs

      let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

      byronWitnesses <-
        fromEitherCli $
          mkShelleyBootstrapWitnesses sbe mNetworkId apiTxBody sksByron

      let newShelleyKeyWits = map (makeShelleyKeyWitness sbe apiTxBody) sksShelley
          allKeyWits = newShelleyKeyWits ++ byronWitnesses

      signedTx <-
        fromEitherCli $
          createCompatibleSignedTx sbe ins allOuts allKeyWits fee protocolUpdates votes txCerts

      fromEitherIOCli $
        writeTxFileTextEnvelopeCddl sbe outputFp signedTx
   where
    validateTxInsReference
      :: [TxIn]
      -> Either TxCmdError (TxInsReference era)
    validateTxInsReference [] = return TxInsReferenceNone
    validateTxInsReference allRefIns = do
      let era = toCardanoEra era
          eraMismatchError = Left $ TxCmdTxFeatureMismatch (anyCardanoEra era) TxFeatureReferenceInputs
      w <- maybe eraMismatchError Right $ forEraMaybeEon era
      pure $ TxInsReference w allRefIns

readUpdateProposalFile
  :: Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)
  -> ExceptT CompatibleTransactionError IO (AnyProtocolUpdate era)
readUpdateProposalFile (Featured sToB Nothing) =
  return $ NoPParamsUpdate $ convert sToB
readUpdateProposalFile (Featured sToB (Just updateProposalFile)) = do
  prop <- firstExceptT CompatibleFileError $ readTxUpdateProposal sToB updateProposalFile
  case prop of
    TxUpdateProposalNone -> return $ NoPParamsUpdate $ convert sToB
    TxUpdateProposal _ proposal -> return $ ProtocolUpdate sToB proposal

readProposalProcedureFile
  :: Featured ConwayEraOnwards era [(ProposalFile In, Maybe CliProposalScriptRequirements)]
  -> ExceptT CompatibleTransactionError IO (AnyProtocolUpdate era)
readProposalProcedureFile (Featured cEraOnwards []) =
  let sbe = convert cEraOnwards
   in return $ NoPParamsUpdate sbe
readProposalProcedureFile (Featured cEraOnwards proposals) = do
  props <-
    mapM
      (firstExceptT CompatibleProposalError . newExceptT . readProposal cEraOnwards)
      proposals
  return $
    conwayEraOnwardsConstraints cEraOnwards $
      ProposalProcedures cEraOnwards $
        mkTxProposalProcedures
          [(govProp, pswScriptWitness <$> mScriptWit) | (Proposal govProp, mScriptWit) <- props]
