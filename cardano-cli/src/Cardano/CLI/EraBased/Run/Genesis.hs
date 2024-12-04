{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.EraBased.Run.Genesis
  ( runGenesisCmds
  , runGenesisAddrCmd
  , runGenesisCreateCardanoCmd
  , runGenesisCreateCmd
  , runGenesisCreateStakedCmd
  , runGenesisHashFileCmd
  , runGenesisKeyHashCmd
  , runGenesisTxInCmd
  , runGenesisVerKeyCmd
  )
where

import           Cardano.Api
import           Cardano.Api.Byron (toByronLovelace, toByronProtocolMagicId,
                   toByronRequiresNetworkMagic)
import qualified Cardano.Api.Byron as Byron hiding (GenesisParameters, SigningKey)
import           Cardano.Api.Consensus (ShelleyGenesisStaking (..))
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Byron.Delegation
import           Cardano.CLI.Byron.Genesis as Byron
import qualified Cardano.CLI.Byron.Key as Byron
import qualified Cardano.CLI.Commands.Node as Cmd
import           Cardano.CLI.EraBased.Commands.Genesis as Cmd
import           Cardano.CLI.EraBased.Run.Genesis.Common
import           Cardano.CLI.EraBased.Run.Genesis.CreateTestnetData (WriteFileGenesis (..))
import qualified Cardano.CLI.EraBased.Run.Genesis.CreateTestnetData as TN
import           Cardano.CLI.EraBased.Run.StakeAddress (runStakeAddressKeyGenCmd)
import qualified Cardano.CLI.IO.Lazy as Lazy
import           Cardano.CLI.Run.Node (runNodeIssueOpCertCmd, runNodeKeyGenColdCmd,
                   runNodeKeyGenKesCmd, runNodeKeyGenVrfCmd)
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GenesisCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.StakePoolCmdError
import           Cardano.CLI.Types.Key
import qualified Cardano.Crypto as CC
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Signing as Byron
import           Cardano.Slotting.Slot (EpochSize (EpochSize))

import           Control.DeepSeq (NFData, force)
import           Control.Exception (evaluate)
import           Control.Monad (forM, forM_, unless, when)
import           Data.Aeson hiding (Key)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (isDigit)
import           Data.Fixed (Fixed (MkFixed))
import           Data.Function (on)
import           Data.Functor (void)
import qualified Data.List as List
import qualified Data.List.Split as List
import           Data.ListMap (ListMap (..))
import qualified Data.ListMap as ListMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Word (Word64)
import qualified Data.Yaml as Yaml
import           GHC.Exts (IsList (..))
import           GHC.Generics (Generic)
import           Lens.Micro ((^.))
import           System.Directory (createDirectoryIfMissing, listDirectory)
import           System.FilePath (takeExtension, takeExtensions, (</>))
import qualified System.IO as IO
import           System.IO.Error (isDoesNotExistError)
import qualified System.Random as Random
import           System.Random (StdGen)
import           Text.Read (readMaybe)

runGenesisCmds :: GenesisCmds era -> ExceptT GenesisCmdError IO ()
runGenesisCmds = \case
  GenesisKeyGenGenesis args -> TN.runGenesisKeyGenGenesisCmd args
  GenesisKeyGenDelegate args -> TN.runGenesisKeyGenDelegateCmd args
  GenesisKeyGenUTxO args -> TN.runGenesisKeyGenUTxOCmd args
  GenesisCmdKeyHash vk -> runGenesisKeyHashCmd vk
  GenesisVerKey args -> runGenesisVerKeyCmd args
  GenesisTxIn args -> runGenesisTxInCmd args
  GenesisAddr args -> runGenesisAddrCmd args
  GenesisCreate args -> runGenesisCreateCmd args
  GenesisCreateCardano args -> runGenesisCreateCardanoCmd args
  GenesisCreateStaked args -> runGenesisCreateStakedCmd args
  GenesisCreateTestNetData args -> TN.runGenesisCreateTestNetDataCmd args
  GenesisHashFile gf -> runGenesisHashFileCmd gf

runGenesisKeyHashCmd :: VerificationKeyFile In -> ExceptT GenesisCmdError IO ()
runGenesisKeyHashCmd vkeyPath = do
  vkey <-
    firstExceptT GenesisCmdTextEnvReadFileError . newExceptT $
      readFileTextEnvelopeAnyOf
        [ FromSomeType
            (AsVerificationKey AsGenesisKey)
            AGenesisKey
        , FromSomeType
            (AsVerificationKey AsGenesisDelegateKey)
            AGenesisDelegateKey
        , FromSomeType
            (AsVerificationKey AsGenesisUTxOKey)
            AGenesisUTxOKey
        ]
        vkeyPath
  liftIO $ BS.putStrLn (renderKeyHash vkey)
 where
  renderKeyHash :: SomeGenesisKey VerificationKey -> ByteString
  renderKeyHash (AGenesisKey vk) = renderVerificationKeyHash vk
  renderKeyHash (AGenesisDelegateKey vk) = renderVerificationKeyHash vk
  renderKeyHash (AGenesisUTxOKey vk) = renderVerificationKeyHash vk

  renderVerificationKeyHash :: Key keyrole => VerificationKey keyrole -> ByteString
  renderVerificationKeyHash =
    serialiseToRawBytesHex
      . verificationKeyHash

runGenesisVerKeyCmd
  :: GenesisVerKeyCmdArgs
  -> ExceptT GenesisCmdError IO ()
runGenesisVerKeyCmd
  Cmd.GenesisVerKeyCmdArgs
    { Cmd.verificationKeyPath
    , Cmd.signingKeyPath
    } = do
    skey <-
      firstExceptT GenesisCmdTextEnvReadFileError . newExceptT $
        readFileTextEnvelopeAnyOf
          [ FromSomeType
              (AsSigningKey AsGenesisKey)
              AGenesisKey
          , FromSomeType
              (AsSigningKey AsGenesisDelegateKey)
              AGenesisDelegateKey
          , FromSomeType
              (AsSigningKey AsGenesisUTxOKey)
              AGenesisUTxOKey
          ]
          signingKeyPath

    let vkey :: SomeGenesisKey VerificationKey
        vkey = case skey of
          AGenesisKey sk -> AGenesisKey (getVerificationKey sk)
          AGenesisDelegateKey sk -> AGenesisDelegateKey (getVerificationKey sk)
          AGenesisUTxOKey sk -> AGenesisUTxOKey (getVerificationKey sk)

    firstExceptT GenesisCmdGenesisFileError . newExceptT . liftIO $
      case vkey of
        AGenesisKey vk -> writeLazyByteStringFile verificationKeyPath $ textEnvelopeToJSON Nothing vk
        AGenesisDelegateKey vk -> writeLazyByteStringFile verificationKeyPath $ textEnvelopeToJSON Nothing vk
        AGenesisUTxOKey vk -> writeLazyByteStringFile verificationKeyPath $ textEnvelopeToJSON Nothing vk

data SomeGenesisKey f
  = AGenesisKey (f GenesisKey)
  | AGenesisDelegateKey (f GenesisDelegateKey)
  | AGenesisUTxOKey (f GenesisUTxOKey)

runGenesisTxInCmd
  :: GenesisTxInCmdArgs
  -> ExceptT GenesisCmdError IO ()
runGenesisTxInCmd
  Cmd.GenesisTxInCmdArgs
    { Cmd.verificationKeyPath
    , Cmd.network
    , Cmd.mOutFile
    } = do
    vkey <-
      firstExceptT GenesisCmdTextEnvReadFileError . newExceptT $
        readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) verificationKeyPath
    let txin = genesisUTxOPseudoTxIn network (verificationKeyHash vkey)
    liftIO $ writeOutput mOutFile (renderTxIn txin)

runGenesisAddrCmd
  :: GenesisAddrCmdArgs
  -> ExceptT GenesisCmdError IO ()
runGenesisAddrCmd
  Cmd.GenesisAddrCmdArgs
    { Cmd.verificationKeyPath
    , Cmd.network
    , Cmd.mOutFile
    } = do
    vkey <-
      firstExceptT GenesisCmdTextEnvReadFileError . newExceptT $
        readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) verificationKeyPath
    let vkh = verificationKeyHash (castVerificationKey vkey)
        addr =
          makeShelleyAddress
            network
            (PaymentCredentialByKey vkh)
            NoStakeAddress
    liftIO $ writeOutput mOutFile (serialiseAddress addr)

writeOutput :: Maybe (File () Out) -> Text -> IO ()
writeOutput (Just (File fpath)) = Text.writeFile fpath
writeOutput Nothing = Text.putStrLn

--
-- Create Genesis command implementation
--

runGenesisCreateCmd
  :: GenesisCreateCmdArgs era
  -> ExceptT GenesisCmdError IO ()
runGenesisCreateCmd
  Cmd.GenesisCreateCmdArgs
    { Cmd.eon
    , Cmd.keyOutputFormat
    , Cmd.genesisDir
    , Cmd.numGenesisKeys
    , Cmd.numUTxOKeys
    , Cmd.mSystemStart
    , Cmd.mSupply
    , Cmd.network
    } = do
    let GenesisDir rootdir = genesisDir
        gendir = rootdir </> "genesis-keys"
        deldir = rootdir </> "delegate-keys"
        utxodir = rootdir </> "utxo-keys"
        era = toCardanoEra eon
    liftIO $ do
      createDirectoryIfMissing False rootdir
      createDirectoryIfMissing False gendir
      createDirectoryIfMissing False deldir
      createDirectoryIfMissing False utxodir

    template <- decodeShelleyGenesisWithDefault (rootdir </> "genesis.spec.json") adjustTemplate
    alonzoGenesis <- decodeAlonzoGenesisFile (Just era) $ rootdir </> "genesis.alonzo.spec.json"
    conwayGenesis <- decodeConwayGenesisFile $ rootdir </> "genesis.conway.spec.json"

    forM_ [1 .. numGenesisKeys] $ \index -> do
      createGenesisKeys gendir index
      createDelegateKeys keyOutputFormat deldir index

    forM_ [1 .. numUTxOKeys] $ \index ->
      createUtxoKeys utxodir index

    genDlgs <- readGenDelegsMap gendir deldir
    utxoAddrs <- readInitialFundAddresses utxodir network
    start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mSystemStart

    let shelleyGenesis =
          updateTemplate
            -- Shelley genesis parameters
            start
            genDlgs
            mSupply
            utxoAddrs
            mempty
            (L.Coin 0)
            []
            []
            template

    forM_
      [ ("genesis.json", WritePretty shelleyGenesis)
      , ("genesis.alonzo.json", WritePretty alonzoGenesis)
      , ("genesis.conway.json", WritePretty conwayGenesis)
      ]
      $ \(filename, genesis) -> TN.writeFileGenesis (rootdir </> filename) genesis
   where
    -- TODO: rationalise the naming convention on these genesis json files.

    adjustTemplate t = t{sgNetworkMagic = unNetworkMagic (toNetworkMagic network)}

toSKeyJSON :: Key a => SigningKey a -> ByteString
toSKeyJSON = LBS.toStrict . textEnvelopeToJSON Nothing

toVkeyJSON
  :: ()
  => Key a
  => HasTypeProxy a
  => SigningKey a
  -> ByteString
toVkeyJSON = LBS.toStrict . textEnvelopeToJSON Nothing . getVerificationKey

toVkeyJSON' :: Key a => VerificationKey a -> ByteString
toVkeyJSON' = LBS.toStrict . textEnvelopeToJSON Nothing

toOpCert :: (OperationalCertificate, OperationalCertificateIssueCounter) -> ByteString
toOpCert = LBS.toStrict . textEnvelopeToJSON Nothing . fst

toCounter :: (OperationalCertificate, OperationalCertificateIssueCounter) -> ByteString
toCounter = LBS.toStrict . textEnvelopeToJSON Nothing . snd

generateShelleyNodeSecrets
  :: [SigningKey GenesisDelegateExtendedKey]
  -> [VerificationKey GenesisKey]
  -> IO
      ( Map
          (Hash GenesisKey)
          (Hash GenesisDelegateKey, Hash VrfKey)
      , [SigningKey VrfKey]
      , [SigningKey KesKey]
      , [(OperationalCertificate, OperationalCertificateIssueCounter)]
      )
generateShelleyNodeSecrets shelleyDelegateKeys shelleyGenesisvkeys = do
  let
    shelleyDelegatevkeys :: [VerificationKey GenesisDelegateKey]
    shelleyDelegatevkeys = map (castVerificationKey . getVerificationKey) shelleyDelegateKeys
  vrfKeys <- forM shelleyDelegateKeys $ \_ -> generateSigningKey AsVrfKey
  kesKeys <- forM shelleyDelegateKeys $ \_ -> generateSigningKey AsKesKey

  let
    opCertInputs :: [(VerificationKey KesKey, SigningKey GenesisDelegateExtendedKey)]
    opCertInputs = zip (map getVerificationKey kesKeys) shelleyDelegateKeys
    createOpCert
      :: (VerificationKey KesKey, SigningKey GenesisDelegateExtendedKey)
      -> (OperationalCertificate, OperationalCertificateIssueCounter)
    createOpCert (kesKey, delegateKey) = either (error . show) id eResult
     where
      eResult = issueOperationalCertificate kesKey (Right delegateKey) (KESPeriod 0) counter
      counter = OperationalCertificateIssueCounter 0 (convert . getVerificationKey $ delegateKey)
      convert
        :: VerificationKey GenesisDelegateExtendedKey
        -> VerificationKey StakePoolKey
      convert =
        ( castVerificationKey
            :: VerificationKey GenesisDelegateKey
            -> VerificationKey StakePoolKey
        )
          . ( castVerificationKey
                :: VerificationKey GenesisDelegateExtendedKey
                -> VerificationKey GenesisDelegateKey
            )

    opCerts :: [(OperationalCertificate, OperationalCertificateIssueCounter)]
    opCerts = map createOpCert opCertInputs

    vrfvkeys = map getVerificationKey vrfKeys
    combinedMap
      :: [ ( VerificationKey GenesisKey
           , VerificationKey GenesisDelegateKey
           , VerificationKey VrfKey
           )
         ]
    combinedMap = zip3 shelleyGenesisvkeys shelleyDelegatevkeys vrfvkeys
    hashKeys
      :: (VerificationKey GenesisKey, VerificationKey GenesisDelegateKey, VerificationKey VrfKey)
      -> (Hash GenesisKey, (Hash GenesisDelegateKey, Hash VrfKey))
    hashKeys (genesis, delegate, vrf) = (verificationKeyHash genesis, (verificationKeyHash delegate, verificationKeyHash vrf))
    delegateMap :: Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey)
    delegateMap = fromList . map hashKeys $ combinedMap

  return (delegateMap, vrfKeys, kesKeys, opCerts)

--
-- Create Genesis Cardano command implementation
--

runGenesisCreateCardanoCmd
  :: GenesisCreateCardanoCmdArgs era
  -> ExceptT GenesisCmdError IO ()
runGenesisCreateCardanoCmd
  Cmd.GenesisCreateCardanoCmdArgs
    { Cmd.eon
    , Cmd.genesisDir
    , Cmd.numGenesisKeys
    , Cmd.numUTxOKeys
    , Cmd.mSystemStart
    , Cmd.mSupply
    , Cmd.security
    , Cmd.slotLength
    , Cmd.slotCoeff
    , Cmd.network
    , Cmd.byronGenesisTemplate
    , Cmd.shelleyGenesisTemplate
    , Cmd.alonzoGenesisTemplate
    , Cmd.conwayGenesisTemplate
    , Cmd.mNodeConfigTemplate
    } = do
    start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mSystemStart
    (byronGenesis', byronSecrets) <- convertToShelleyError $ Byron.mkGenesis $ byronParams start
    let
      byronGenesis =
        byronGenesis'
          { Byron.gdProtocolParameters =
              (Byron.gdProtocolParameters byronGenesis')
                { Byron.ppSlotDuration = floor (toRational slotLength * recip slotCoeff)
                }
          }

      genesisKeys = Byron.gsDlgIssuersSecrets byronSecrets
      byronGenesisKeys = map ByronSigningKey genesisKeys
      shelleyGenesisKeys = map convertGenesisKey genesisKeys
      shelleyGenesisvkeys :: [VerificationKey GenesisKey]
      shelleyGenesisvkeys = map (castVerificationKey . getVerificationKey) shelleyGenesisKeys

      delegateKeys = Byron.gsRichSecrets byronSecrets
      byronDelegateKeys = map ByronSigningKey delegateKeys
      shelleyDelegateKeys :: [SigningKey GenesisDelegateExtendedKey]
      shelleyDelegateKeys = map convertDelegate delegateKeys
      shelleyDelegatevkeys :: [VerificationKey GenesisDelegateKey]
      shelleyDelegatevkeys = map (castVerificationKey . getVerificationKey) shelleyDelegateKeys

      utxoKeys = Byron.gsPoorSecrets byronSecrets
      byronUtxoKeys = map (ByronSigningKey . Byron.poorSecretToKey) utxoKeys
      shelleyUtxoKeys = map (convertPoor . Byron.poorSecretToKey) utxoKeys
      era = toCardanoEra eon

    dlgCerts <- convertToShelleyError $ mapM (findDelegateCert byronGenesis) byronDelegateKeys
    let
      overrideShelleyGenesis t =
        t
          { sgNetworkMagic = unNetworkMagic (toNetworkMagic network)
          , sgNetworkId = toShelleyNetwork network
          , sgActiveSlotsCoeff = unsafeBoundedRational slotCoeff
          , sgSecurityParam = Byron.unBlockCount security
          , sgUpdateQuorum = fromIntegral $ ((numGenesisKeys `div` 3) * 2) + 1
          , sgEpochLength = EpochSize $ floor $ (fromIntegral (Byron.unBlockCount security) * 10) / slotCoeff
          , sgMaxLovelaceSupply = 45_000_000_000_000_000
          , sgSystemStart = getSystemStart start
          , sgSlotLength = L.secondsToNominalDiffTimeMicro $ MkFixed (fromIntegral slotLength) * 1_000
          }
    shelleyGenesisTemplate' <-
      overrideShelleyGenesis <$> decodeShelleyGenesisFile shelleyGenesisTemplate
    alonzoGenesis <- decodeAlonzoGenesisFile (Just era) alonzoGenesisTemplate
    conwayGenesis <- decodeConwayGenesisFile conwayGenesisTemplate
    (delegateMap, vrfKeys, kesKeys, opCerts) <-
      liftIO $ generateShelleyNodeSecrets shelleyDelegateKeys shelleyGenesisvkeys
    let
      shelleyGenesis :: ShelleyGenesis L.StandardCrypto
      shelleyGenesis = updateTemplate start delegateMap Nothing [] mempty 0 [] [] shelleyGenesisTemplate'

    let GenesisDir rootdir = genesisDir
        gendir = rootdir </> "genesis-keys"
        deldir = rootdir </> "delegate-keys"
        utxodir = rootdir </> "utxo-keys"

    liftIO $ do
      createDirectoryIfMissing False rootdir
      createDirectoryIfMissing False gendir
      createDirectoryIfMissing False deldir
      createDirectoryIfMissing False utxodir

      writeSecrets gendir "byron" "key" serialiseToRawBytes byronGenesisKeys
      writeSecrets gendir "shelley" "skey" toSKeyJSON shelleyGenesisKeys
      writeSecrets gendir "shelley" "vkey" toVkeyJSON shelleyGenesisKeys

      writeSecrets deldir "byron" "key" serialiseToRawBytes byronDelegateKeys
      writeSecrets deldir "shelley" "skey" toSKeyJSON shelleyDelegateKeys
      writeSecrets deldir "shelley" "vkey" toVkeyJSON' shelleyDelegatevkeys
      writeSecrets deldir "shelley" "vrf.skey" toSKeyJSON vrfKeys
      writeSecrets deldir "shelley" "vrf.vkey" toVkeyJSON vrfKeys
      writeSecrets deldir "shelley" "kes.skey" toSKeyJSON kesKeys
      writeSecrets deldir "shelley" "kes.vkey" toVkeyJSON kesKeys

      writeSecrets utxodir "byron" "key" serialiseToRawBytes byronUtxoKeys
      writeSecrets utxodir "shelley" "skey" toSKeyJSON shelleyUtxoKeys
      writeSecrets utxodir "shelley" "vkey" toVkeyJSON shelleyUtxoKeys

      writeSecrets deldir "byron" "cert.json" serialiseDelegationCert dlgCerts

      writeSecrets deldir "shelley" "opcert.json" toOpCert opCerts
      writeSecrets deldir "shelley" "counter.json" toCounter opCerts

    byronGenesisHash <-
      TN.writeFileGenesis (rootdir </> "byron-genesis.json") $ WriteCanonical byronGenesis
    shelleyGenesisHash <-
      TN.writeFileGenesis (rootdir </> "shelley-genesis.json") $ WritePretty shelleyGenesis
    alonzoGenesisHash <-
      TN.writeFileGenesis (rootdir </> "alonzo-genesis.json") $ WritePretty alonzoGenesis
    conwayGenesisHash <-
      TN.writeFileGenesis (rootdir </> "conway-genesis.json") $ WritePretty conwayGenesis

    liftIO $ do
      case mNodeConfigTemplate of
        Nothing -> pure ()
        Just nodeCfg -> do
          let hashes =
                Map.fromList
                  [ ("ByronGenesisHash", byronGenesisHash)
                  , ("ShelleyGenesisHash", shelleyGenesisHash)
                  , ("AlonzoGenesisHash", alonzoGenesisHash)
                  , ("ConwayGenesisHash", conwayGenesisHash)
                  ]
          writeGenesisHashesToNodeConfigFile nodeCfg hashes (rootdir </> "node-config.json")
   where
    convertToShelleyError = withExceptT GenesisCmdByronError
    convertGenesisKey :: Byron.SigningKey -> SigningKey GenesisExtendedKey
    convertGenesisKey (Byron.SigningKey xsk) = GenesisExtendedSigningKey xsk

    convertDelegate :: Byron.SigningKey -> SigningKey GenesisDelegateExtendedKey
    convertDelegate (Byron.SigningKey xsk) = GenesisDelegateExtendedSigningKey xsk

    convertPoor :: Byron.SigningKey -> SigningKey ByronKey
    convertPoor = ByronSigningKey

    byronParams start =
      Byron.GenesisParameters
        (getSystemStart start)
        byronGenesisTemplate
        security
        byronNetwork
        byronBalance
        byronFakeAvvm
        byronAvvmFactor
        Nothing
    byronNetwork =
      CC.AProtocolMagic
        (L.Annotated (toByronProtocolMagicId network) ())
        (toByronRequiresNetworkMagic network)
    byronBalance =
      Byron.TestnetBalanceOptions
        { tboRichmen = numGenesisKeys
        , tboPoors = numUTxOKeys
        , tboTotalBalance = fromMaybe zeroLovelace $ toByronLovelace (fromMaybe 0 mSupply)
        , tboRichmenShare = 0
        }
    byronFakeAvvm =
      Byron.FakeAvvmOptions
        { faoCount = 0
        , faoOneBalance = zeroLovelace
        }
    byronAvvmFactor = Byron.rationalToLovelacePortion 0.0
    zeroLovelace = Byron.mkKnownLovelace @0

    -- Compare a given 'SigningKey' with a 'Certificate' 'VerificationKey'
    isCertForSK :: CC.SigningKey -> Byron.Certificate -> Bool
    isCertForSK sk cert = Byron.delegateVK cert == CC.toVerification sk

    findDelegateCert
      :: Byron.GenesisData -> SigningKey ByronKey -> ExceptT ByronGenesisError IO Byron.Certificate
    findDelegateCert byronGenesis bSkey@(ByronSigningKey sk) = do
      case List.find (isCertForSK sk) (Map.elems $ dlgCertMap byronGenesis) of
        Nothing ->
          throwE
            . NoGenesisDelegationForKey
            . Byron.prettyPublicKey
            $ getVerificationKey bSkey
        Just x -> pure x

    dlgCertMap :: Byron.GenesisData -> Map Byron.KeyHash Byron.Certificate
    dlgCertMap byronGenesis = Byron.unGenesisDelegation $ Byron.gdHeavyDelegation byronGenesis

-- | @writeGenesisHashesToNodeConfigFile src hashes dest@ reads the node configuration file
-- at @src@ and the writes an augmented version of this file at @dest@, with the hashes.
writeGenesisHashesToNodeConfigFile
  :: MonadIO m
  => FilePath
  -- ^ From where to read the node configuration file
  -> Map.Map Aeson.Key (Crypto.Hash h a)
  -- ^ Key of an era's hash (like "ByronGenesisHash", "ShelleyGenesisHash", etc.), to the hash of its genesis file
  -> FilePath
  -- ^ Where to write the updated node config file
  -> m ()
writeGenesisHashesToNodeConfigFile sourcePath hashes destinationPath = do
  nodeConfig <- Yaml.decodeFileThrow sourcePath
  let newConfig = foldr updateConfigHash nodeConfig $ Map.toList hashes
  liftIO $ Aeson.encodeFile destinationPath newConfig
 where
  setHash field hash = Aeson.insert field $ Aeson.String $ Crypto.hashToTextAsHex hash
  updateConfigHash :: (Aeson.Key, Crypto.Hash h a) -> Yaml.Value -> Yaml.Value
  updateConfigHash (field, hash) =
    \case
      Aeson.Object obj -> Aeson.Object $ setHash field hash obj
      v -> v

runGenesisCreateStakedCmd
  :: GenesisCreateStakedCmdArgs era
  -> ExceptT GenesisCmdError IO ()
runGenesisCreateStakedCmd
  Cmd.GenesisCreateStakedCmdArgs
    { eon
    , Cmd.keyOutputFormat
    , Cmd.genesisDir
    , Cmd.numGenesisKeys
    , Cmd.numUTxOKeys
    , Cmd.numPools
    , Cmd.numStakeDelegators
    , Cmd.mSystemStart
    , Cmd.mNonDelegatedSupply
    , Cmd.delegatedSupply
    , Cmd.network = networkId
    , Cmd.numBulkPoolCredFiles
    , Cmd.numBulkPoolsPerFile
    , Cmd.numStuffedUtxo
    , Cmd.mStakePoolRelaySpecFile
    } = do
    let GenesisDir rootdir = genesisDir
        gendir = rootdir </> "genesis-keys"
        deldir = rootdir </> "delegate-keys"
        pooldir = rootdir </> "pools"
        stdeldir = rootdir </> "stake-delegator-keys"
        utxodir = rootdir </> "utxo-keys"
        era = toCardanoEra eon

    liftIO $ do
      createDirectoryIfMissing False rootdir
      createDirectoryIfMissing False gendir
      createDirectoryIfMissing False deldir
      createDirectoryIfMissing False pooldir
      createDirectoryIfMissing False stdeldir
      createDirectoryIfMissing False utxodir

    template <- decodeShelleyGenesisWithDefault (rootdir </> "genesis.spec.json") adjustTemplate
    alonzoGenesis <- decodeAlonzoGenesisFile (Just era) $ rootdir </> "genesis.alonzo.spec.json"
    conwayGenesis <- decodeConwayGenesisFile $ rootdir </> "genesis.conway.spec.json"

    forM_ [1 .. numGenesisKeys] $ \index -> do
      createGenesisKeys gendir index
      createDelegateKeys keyOutputFormat deldir index

    forM_ [1 .. numUTxOKeys] $ \index ->
      createUtxoKeys utxodir index

    mStakePoolRelays <- forM mStakePoolRelaySpecFile readRelays

    poolParams <- forM [1 .. numPools] $ \index -> do
      createPoolCredentials keyOutputFormat pooldir index
      buildPoolParams networkId pooldir (Just index) (fromMaybe mempty mStakePoolRelays)

    when (numBulkPoolCredFiles * numBulkPoolsPerFile > numPools) $
      left $
        GenesisCmdTooFewPoolsForBulkCreds numPools numBulkPoolCredFiles numBulkPoolsPerFile
    -- We generate the bulk files for the last pool indices,
    -- so that all the non-bulk pools have stable indices at beginning:
    let bulkOffset = fromIntegral $ numPools - numBulkPoolCredFiles * numBulkPoolsPerFile
        bulkIndices :: [Word] = [1 + bulkOffset .. numPools]
        bulkSlices :: [[Word]] = List.chunksOf (fromIntegral numBulkPoolsPerFile) bulkIndices
    forM_ (zip [1 .. numBulkPoolCredFiles] bulkSlices) $
      uncurry (writeBulkPoolCredentials pooldir)

    let (delegsPerPool, delegsRemaining) =
          if numPools == 0
            then (0, 0)
            else numStakeDelegators `divMod` numPools
        delegsForPool poolIx =
          if delegsRemaining /= 0 && poolIx == numPools
            then delegsPerPool
            else delegsPerPool + delegsRemaining
        distribution = [pool | (pool, poolIx) <- zip poolParams [1 ..], _ <- [1 .. delegsForPool poolIx]]

    g <- Random.getStdGen

    -- Distribute M delegates across N pools:
    delegations <- liftIO $ Lazy.forStateM g distribution $ flip computeInsecureDelegation networkId

    let numDelegations = length delegations

    genDlgs <- readGenDelegsMap gendir deldir
    nonDelegAddrs <- readInitialFundAddresses utxodir networkId
    start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mSystemStart

    let network = toShelleyNetwork networkId
    stuffedUtxoAddrs <-
      liftIO $ Lazy.replicateM (fromIntegral numStuffedUtxo) $ genStuffedAddress network

    let stake = second L.ppId . mkDelegationMapEntry <$> delegations
        stakePools = [(L.ppId poolParams', poolParams') | poolParams' <- snd . mkDelegationMapEntry <$> delegations]
        delegAddrs = dInitialUtxoAddr <$> delegations
        !shelleyGenesis =
          updateOutputTemplate
            -- Shelley genesis parameters
            start
            genDlgs
            mNonDelegatedSupply
            (length nonDelegAddrs)
            nonDelegAddrs
            stakePools
            stake
            (Just delegatedSupply)
            numDelegations
            delegAddrs
            stuffedUtxoAddrs
            template

    forM_
      [ ("genesis.json", WritePretty shelleyGenesis)
      , ("genesis.alonzo.json", WritePretty alonzoGenesis)
      , ("genesis.conway.json", WritePretty conwayGenesis)
      ]
      $ \(filename, genesis) -> TN.writeFileGenesis (rootdir </> filename) genesis
    -- TODO: rationalise the naming convention on these genesis json files.

    liftIO $
      Text.hPutStrLn IO.stderr $
        mconcat $
          [ "generated genesis with: "
          , textShow numGenesisKeys
          , " genesis keys, "
          , textShow numUTxOKeys
          , " non-delegating UTxO keys, "
          , textShow numPools
          , " stake pools, "
          , textShow numStakeDelegators
          , " delegating UTxO keys, "
          , textShow numDelegations
          , " delegation map entries, "
          ]
            ++ [ mconcat
                  [ ", "
                  , textShow numBulkPoolCredFiles
                  , " bulk pool credential files, "
                  , textShow numBulkPoolsPerFile
                  , " pools per bulk credential file, indices starting from "
                  , textShow bulkOffset
                  , ", "
                  , textShow $ length bulkIndices
                  , " total pools in bulk nodes, each bulk node having this many entries: "
                  , textShow $ length <$> bulkSlices
                  ]
               | numBulkPoolCredFiles * numBulkPoolsPerFile > 0
               ]
   where
    adjustTemplate t = t{sgNetworkMagic = unNetworkMagic (toNetworkMagic networkId)}
    mkDelegationMapEntry
      :: Delegation -> (L.KeyHash L.Staking L.StandardCrypto, L.PoolParams L.StandardCrypto)
    mkDelegationMapEntry d = (dDelegStaking d, dPoolParams d)

-- -------------------------------------------------------------------------------------------------

updateOutputTemplate
  :: SystemStart
  -- ^ System start time
  -> Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey)
  -- ^ Genesis delegation (not stake-based)
  -> Maybe Lovelace
  -- ^ Amount of lovelace not delegated
  -> Int
  -- ^ Number of UTxO addresses that are delegating
  -> [AddressInEra ShelleyEra]
  -- ^ UTxO addresses that are not delegating
  -> [(L.KeyHash 'L.StakePool L.StandardCrypto, L.PoolParams L.StandardCrypto)]
  -- ^ Pool map
  -> [(L.KeyHash 'L.Staking L.StandardCrypto, L.KeyHash 'L.StakePool L.StandardCrypto)]
  -- ^ Delegaton map
  -> Maybe Lovelace
  -- ^ Amount of lovelace to delegate
  -> Int
  -- ^ Number of UTxO address for delegation
  -> [AddressInEra ShelleyEra]
  -- ^ UTxO address for delegation
  -> [AddressInEra ShelleyEra]
  -- ^ Stuffed UTxO addresses
  -> ShelleyGenesis L.StandardCrypto
  -- ^ Template from which to build a genesis
  -> ShelleyGenesis L.StandardCrypto
  -- ^ Updated genesis
updateOutputTemplate
  (SystemStart sgSystemStart)
  genDelegMap
  mAmountNonDeleg
  nUtxoAddrsNonDeleg
  utxoAddrsNonDeleg
  pools
  stake
  amountDeleg
  nUtxoAddrsDeleg
  utxoAddrsDeleg
  stuffedUtxoAddrs
  template@ShelleyGenesis{sgProtocolParams} =
    template
      { sgSystemStart
      , sgMaxLovelaceSupply = fromIntegral $ nonDelegCoin + delegCoin
      , sgGenDelegs = shelleyDelKeys
      , sgInitialFunds =
          fromList
            [ (toShelleyAddr addr, v)
            | (addr, v) <-
                distribute (nonDelegCoin - subtractForTreasury) nUtxoAddrsNonDeleg utxoAddrsNonDeleg
                  ++ distribute (delegCoin - subtractForTreasury) nUtxoAddrsDeleg utxoAddrsDeleg
                  ++ mkStuffedUtxo stuffedUtxoAddrs
            ]
      , sgStaking =
          ShelleyGenesisStaking
            { sgsPools = ListMap pools
            , sgsStake = ListMap stake
            }
      , sgProtocolParams
      }
   where
    maximumLovelaceSupply :: Word64
    maximumLovelaceSupply = sgMaxLovelaceSupply template
    -- If the initial funds are equal to the maximum funds, rewards cannot be created.
    subtractForTreasury :: Integer
    subtractForTreasury = nonDelegCoin `quot` 10
    nonDelegCoin, delegCoin :: Integer
    -- if --supply is not specified, non delegated supply comes from the template passed to this function:
    nonDelegCoin = fromIntegral (maybe maximumLovelaceSupply unLovelace mAmountNonDeleg)
    delegCoin = maybe 0 fromIntegral amountDeleg

    distribute :: Integer -> Int -> [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    distribute funds nAddrs addrs = zip addrs (fmap L.Coin (coinPerAddr + remainder : repeat coinPerAddr))
     where
      coinPerAddr, remainder :: Integer
      (coinPerAddr, remainder) = funds `divMod` fromIntegral nAddrs

    mkStuffedUtxo :: [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    mkStuffedUtxo xs = (,L.Coin minUtxoVal) <$> xs
     where
      L.Coin minUtxoVal = sgProtocolParams ^. L.ppMinUTxOValueL

    shelleyDelKeys =
      fromList
        [ (gh, L.GenDelegPair gdh $ L.toVRFVerKeyHash h)
        | ( GenesisKeyHash gh
            , (GenesisDelegateKeyHash gdh, VrfKeyHash h)
            ) <-
            toList genDelegMap
        ]

    unLovelace :: Integral a => Lovelace -> a
    unLovelace (L.Coin coin) = fromIntegral coin

createDelegateKeys :: KeyOutputFormat -> FilePath -> Word -> ExceptT GenesisCmdError IO ()
createDelegateKeys fmt dir index = do
  liftIO $ createDirectoryIfMissing False dir
  TN.runGenesisKeyGenDelegateCmd
    Cmd.GenesisKeyGenDelegateCmdArgs
      { Cmd.verificationKeyPath = File @(VerificationKey ()) $ dir </> "delegate" ++ strIndex ++ ".vkey"
      , Cmd.signingKeyPath = onlyOut coldSK
      , Cmd.opCertCounterPath = onlyOut opCertCtr
      }
  TN.runGenesisKeyGenDelegateVRF
    (File @(VerificationKey ()) $ dir </> "delegate" ++ strIndex ++ ".vrf.vkey")
    (File @(SigningKey ()) $ dir </> "delegate" ++ strIndex ++ ".vrf.skey")
  firstExceptT GenesisCmdNodeCmdError $ do
    runNodeKeyGenKesCmd $
      Cmd.NodeKeyGenKESCmdArgs
        fmt
        (onlyOut kesVK)
        (File @(SigningKey ()) $ dir </> "delegate" ++ strIndex ++ ".kes.skey")
    runNodeIssueOpCertCmd $
      Cmd.NodeIssueOpCertCmdArgs
        (VerificationKeyFilePath (onlyIn kesVK))
        (onlyIn coldSK)
        opCertCtr
        (KESPeriod 0)
        (File $ dir </> "opcert" ++ strIndex ++ ".cert")
 where
  strIndex = show index
  kesVK = File @(VerificationKey ()) $ dir </> "delegate" ++ strIndex ++ ".kes.vkey"
  coldSK = File @(SigningKey ()) $ dir </> "delegate" ++ strIndex ++ ".skey"
  opCertCtr = File $ dir </> "delegate" ++ strIndex ++ ".counter"

createGenesisKeys :: FilePath -> Word -> ExceptT GenesisCmdError IO ()
createGenesisKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  TN.runGenesisKeyGenGenesisCmd
    GenesisKeyGenGenesisCmdArgs
      { verificationKeyPath = File @(VerificationKey ()) $ dir </> "genesis" ++ strIndex ++ ".vkey"
      , signingKeyPath = File @(SigningKey ()) $ dir </> "genesis" ++ strIndex ++ ".skey"
      }

createUtxoKeys :: FilePath -> Word -> ExceptT GenesisCmdError IO ()
createUtxoKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  TN.runGenesisKeyGenUTxOCmd
    Cmd.GenesisKeyGenUTxOCmdArgs
      { Cmd.verificationKeyPath = File @(VerificationKey ()) $ dir </> "utxo" ++ strIndex ++ ".vkey"
      , Cmd.signingKeyPath = File @(SigningKey ()) $ dir </> "utxo" ++ strIndex ++ ".skey"
      }

createPoolCredentials :: KeyOutputFormat -> FilePath -> Word -> ExceptT GenesisCmdError IO ()
createPoolCredentials fmt dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT GenesisCmdNodeCmdError $ do
    runNodeKeyGenKesCmd $
      Cmd.NodeKeyGenKESCmdArgs
        fmt
        (onlyOut kesVK)
        (File @(SigningKey ()) $ dir </> "kes" ++ strIndex ++ ".skey")
    runNodeKeyGenVrfCmd $
      Cmd.NodeKeyGenVRFCmdArgs
        fmt
        (File @(VerificationKey ()) $ dir </> "vrf" ++ strIndex ++ ".vkey")
        (File @(SigningKey ()) $ dir </> "vrf" ++ strIndex ++ ".skey")
    runNodeKeyGenColdCmd $
      Cmd.NodeKeyGenColdCmdArgs
        fmt
        (File @(VerificationKey ()) $ dir </> "cold" ++ strIndex ++ ".vkey")
        (onlyOut coldSK)
        (onlyOut opCertCtr)
    runNodeIssueOpCertCmd $
      Cmd.NodeIssueOpCertCmdArgs
        (VerificationKeyFilePath (onlyIn kesVK))
        (onlyIn coldSK)
        opCertCtr
        (KESPeriod 0)
        (File $ dir </> "opcert" ++ strIndex ++ ".cert")
  firstExceptT GenesisCmdStakeAddressCmdError $
    void $
      runStakeAddressKeyGenCmd
        fmt
        (File @(VerificationKey ()) $ dir </> "staking-reward" ++ strIndex ++ ".vkey")
        (File @(SigningKey ()) $ dir </> "staking-reward" ++ strIndex ++ ".skey")
 where
  strIndex = show index
  kesVK = File @(VerificationKey ()) $ dir </> "kes" ++ strIndex ++ ".vkey"
  coldSK = File @(SigningKey ()) $ dir </> "cold" ++ strIndex ++ ".skey"
  opCertCtr = File $ dir </> "opcert" ++ strIndex ++ ".counter"

data Delegation = Delegation
  { dInitialUtxoAddr :: !(AddressInEra ShelleyEra)
  , dDelegStaking :: !(L.KeyHash L.Staking L.StandardCrypto)
  , dPoolParams :: !(L.PoolParams L.StandardCrypto)
  }
  deriving (Generic, NFData)

buildPoolParams
  :: NetworkId
  -> FilePath
  -- ^ File directory where the necessary pool credentials were created
  -> Maybe Word
  -> Map Word [L.StakePoolRelay]
  -- ^ User submitted stake pool relay map
  -> ExceptT GenesisCmdError IO (L.PoolParams L.StandardCrypto)
buildPoolParams nw dir index specifiedRelays = do
  StakePoolVerificationKey poolColdVK <-
    firstExceptT (GenesisCmdStakePoolCmdError . StakePoolCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) poolColdVKF

  VrfVerificationKey poolVrfVK <-
    firstExceptT (GenesisCmdNodeCmdError . NodeCmdReadFileError)
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsVrfKey) poolVrfVKF
  rewardsSVK <-
    firstExceptT GenesisCmdTextEnvReadFileError
      . newExceptT
      $ readFileTextEnvelope (AsVerificationKey AsStakeKey) poolRewardVKF

  pure
    L.PoolParams
      { L.ppId = L.hashKey poolColdVK
      , L.ppVrf = L.hashVerKeyVRF poolVrfVK
      , L.ppPledge = L.Coin 0
      , L.ppCost = L.Coin 0
      , L.ppMargin = minBound
      , L.ppRewardAccount =
          toShelleyStakeAddr $ makeStakeAddress nw $ StakeCredentialByKey (verificationKeyHash rewardsSVK)
      , L.ppOwners = mempty
      , L.ppRelays = lookupPoolRelay specifiedRelays
      , L.ppMetadata = L.SNothing
      }
 where
  lookupPoolRelay
    :: Map Word [L.StakePoolRelay] -> Seq.StrictSeq L.StakePoolRelay
  lookupPoolRelay m =
    case index of
      Nothing -> mempty
      Just index' -> maybe mempty fromList (Map.lookup index' m)

  strIndex = maybe "" show index
  poolColdVKF = File $ dir </> "cold" ++ strIndex ++ ".vkey"
  poolVrfVKF = File $ dir </> "vrf" ++ strIndex ++ ".vkey"
  poolRewardVKF = File $ dir </> "staking-reward" ++ strIndex ++ ".vkey"

writeBulkPoolCredentials :: FilePath -> Word -> [Word] -> ExceptT GenesisCmdError IO ()
writeBulkPoolCredentials dir bulkIx poolIxs = do
  creds <- mapM readPoolCreds poolIxs
  handleIOExceptT (GenesisCmdFileError . FileIOError bulkFile) $
    LBS.writeFile bulkFile $
      Aeson.encode creds
 where
  bulkFile = dir </> "bulk" ++ show bulkIx ++ ".creds"

  readPoolCreds
    :: Word
    -> ExceptT
        GenesisCmdError
        IO
        (TextEnvelope, TextEnvelope, TextEnvelope)
  readPoolCreds ix = do
    (,,)
      <$> readEnvelope poolOpCert
      <*> readEnvelope poolVrfSKF
      <*> readEnvelope poolKesSKF
   where
    strIndex = show ix
    poolOpCert = dir </> "opcert" ++ strIndex ++ ".cert"
    poolVrfSKF = dir </> "vrf" ++ strIndex ++ ".skey"
    poolKesSKF = dir </> "kes" ++ strIndex ++ ".skey"
  readEnvelope :: FilePath -> ExceptT GenesisCmdError IO TextEnvelope
  readEnvelope fp = do
    content <-
      handleIOExceptT (GenesisCmdFileError . FileIOError fp) $
        BS.readFile fp
    firstExceptT (GenesisCmdFileDecodeError fp . Text.pack) . hoistEither $
      Aeson.eitherDecodeStrict' content

-- | This function should only be used for testing purposes.
-- Keys returned by this function are not cryptographically secure.
computeInsecureDelegation
  :: StdGen
  -> NetworkId
  -> L.PoolParams L.StandardCrypto
  -> IO (StdGen, Delegation)
computeInsecureDelegation g0 nw pool = do
  (paymentVK, g1) <- first getVerificationKey <$> generateInsecureSigningKey g0 AsPaymentKey
  (stakeVK, g2) <- first getVerificationKey <$> generateInsecureSigningKey g1 AsStakeKey

  let stakeAddressReference = StakeAddressByValue . StakeCredentialByKey . verificationKeyHash $ stakeVK
      initialUtxoAddr =
        makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash paymentVK)) stakeAddressReference

      delegation =
        Delegation
          { dInitialUtxoAddr = shelleyAddressInEra ShelleyBasedEraShelley initialUtxoAddr
          , dDelegStaking = L.hashKey (unStakeVerificationKey stakeVK)
          , dPoolParams = pool
          }

  evaluate . force $ (g2, delegation)

-- | Attempts to read Shelley genesis from disk
-- and if not found creates a default Shelley genesis.
decodeShelleyGenesisWithDefault
  :: FilePath
  -> (ShelleyGenesis L.StandardCrypto -> ShelleyGenesis L.StandardCrypto)
  -> ExceptT GenesisCmdError IO (ShelleyGenesis L.StandardCrypto)
decodeShelleyGenesisWithDefault fpath adjustDefaults = do
  decodeShelleyGenesisFile fpath
    `catchError` \err ->
      case err of
        GenesisCmdGenesisFileError (FileIOError _ ioe)
          | isDoesNotExistError ioe -> writeDefault
        _ -> left err
 where
  defaults :: ShelleyGenesis L.StandardCrypto
  defaults = adjustDefaults shelleyGenesisDefaults

  writeDefault = do
    handleIOExceptT (GenesisCmdGenesisFileError . FileIOError fpath) $
      LBS.writeFile fpath (encode defaults)
    return defaults

updateTemplate
  :: SystemStart
  -- ^ System start time
  -> Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey)
  -- ^ Genesis delegation (not stake-based)
  -> Maybe Lovelace
  -- ^ Amount of lovelace not delegated
  -> [AddressInEra ShelleyEra]
  -- ^ UTxO addresses that are not delegating
  -> Map (L.KeyHash 'L.Staking L.StandardCrypto) (L.PoolParams L.StandardCrypto)
  -- ^ Genesis staking: pools/delegation map & delegated initial UTxO spec
  -> Lovelace
  -- ^ Number of UTxO Addresses for delegation
  -> [AddressInEra ShelleyEra]
  -- ^ UTxO Addresses for delegation
  -> [AddressInEra ShelleyEra]
  -- ^ Stuffed UTxO addresses
  -> ShelleyGenesis L.StandardCrypto
  -- ^ Template from which to build a genesis
  -> ShelleyGenesis L.StandardCrypto
  -- ^ Updated genesis
updateTemplate
  (SystemStart start)
  genDelegMap
  mAmountNonDeleg
  utxoAddrsNonDeleg
  poolSpecs
  (L.Coin amountDeleg)
  utxoAddrsDeleg
  stuffedUtxoAddrs
  template = do
    let pparamsFromTemplate = sgProtocolParams template
        shelleyGenesis =
          template
            { sgSystemStart = start
            , sgMaxLovelaceSupply = fromIntegral $ nonDelegCoin + delegCoin
            , sgGenDelegs = shelleyDelKeys
            , sgInitialFunds =
                fromList
                  [ (toShelleyAddr addr, v)
                  | (addr, v) <-
                      distribute (nonDelegCoin - subtractForTreasury) utxoAddrsNonDeleg
                        ++ distribute (delegCoin - subtractForTreasury) utxoAddrsDeleg
                        ++ mkStuffedUtxo stuffedUtxoAddrs
                  ]
            , sgStaking =
                ShelleyGenesisStaking
                  { sgsPools =
                      fromList
                        [ (L.ppId poolParams, poolParams)
                        | poolParams <- Map.elems poolSpecs
                        ]
                  , sgsStake = ListMap.fromMap $ L.ppId <$> poolSpecs
                  }
            , sgProtocolParams = pparamsFromTemplate
            }
    shelleyGenesis
   where
    maximumLovelaceSupply :: Word64
    maximumLovelaceSupply = sgMaxLovelaceSupply template
    -- If the initial funds are equal to the maximum funds, rewards cannot be created.
    subtractForTreasury :: Integer
    subtractForTreasury = nonDelegCoin `quot` 10
    nonDelegCoin, delegCoin :: Integer
    nonDelegCoin = fromIntegral (maybe maximumLovelaceSupply unLovelace mAmountNonDeleg)
    delegCoin = fromIntegral amountDeleg

    distribute :: Integer -> [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    distribute funds addrs =
      fst $ List.foldl' folder ([], fromIntegral funds) addrs
     where
      nAddrs, coinPerAddr, splitThreshold :: Integer
      nAddrs = fromIntegral $ length addrs
      coinPerAddr = funds `div` nAddrs
      splitThreshold = coinPerAddr + nAddrs

      folder
        :: ([(AddressInEra ShelleyEra, Lovelace)], Integer)
        -> AddressInEra ShelleyEra
        -> ([(AddressInEra ShelleyEra, Lovelace)], Integer)
      folder (acc, rest) addr
        | rest > splitThreshold =
            ((addr, L.Coin coinPerAddr) : acc, rest - coinPerAddr)
        | otherwise = ((addr, L.Coin rest) : acc, 0)

    mkStuffedUtxo :: [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    mkStuffedUtxo xs = (,L.Coin minUtxoVal) <$> xs
     where
      L.Coin minUtxoVal = sgProtocolParams template ^. L.ppMinUTxOValueL

    shelleyDelKeys =
      fromList
        [ (gh, L.GenDelegPair gdh $ L.toVRFVerKeyHash h)
        | ( GenesisKeyHash gh
            , (GenesisDelegateKeyHash gdh, VrfKeyHash h)
            ) <-
            toList genDelegMap
        ]

    unLovelace :: Integral a => Lovelace -> a
    unLovelace (L.Coin coin) = fromIntegral coin

-- ----------------------------------------------------------------------------

readGenDelegsMap
  :: FilePath
  -> FilePath
  -> ExceptT
      GenesisCmdError
      IO
      ( Map
          (Hash GenesisKey)
          (Hash GenesisDelegateKey, Hash VrfKey)
      )
readGenDelegsMap gendir deldir = do
  gkm <- readGenesisKeys gendir
  dkm <- readDelegateKeys deldir
  vkm <- readDelegateVrfKeys deldir

  let combinedMap
        :: Map
            Int
            ( VerificationKey GenesisKey
            , ( VerificationKey GenesisDelegateKey
              , VerificationKey VrfKey
              )
            )
      combinedMap =
        Map.intersectionWith
          (,)
          gkm
          ( Map.intersectionWith
              (,)
              dkm
              vkm
          )

  -- All the maps should have an identical set of keys. Complain if not.
  let gkmExtra = gkm Map.\\ combinedMap
      dkmExtra = dkm Map.\\ combinedMap
      vkmExtra = vkm Map.\\ combinedMap
  unless (Map.null gkmExtra && Map.null dkmExtra && Map.null vkmExtra) $
    throwError $
      GenesisCmdMismatchedGenesisKeyFiles
        (Map.keys gkm)
        (Map.keys dkm)
        (Map.keys vkm)

  let delegsMap
        :: Map
            (Hash GenesisKey)
            (Hash GenesisDelegateKey, Hash VrfKey)
      delegsMap =
        fromList
          [ (gh, (dh, vh))
          | (g, (d, v)) <- Map.elems combinedMap
          , let gh = verificationKeyHash g
                dh = verificationKeyHash d
                vh = verificationKeyHash v
          ]

  pure delegsMap

readGenesisKeys
  :: FilePath
  -> ExceptT
      GenesisCmdError
      IO
      (Map Int (VerificationKey GenesisKey))
readGenesisKeys gendir = do
  files <- liftIO (listDirectory gendir)
  fileIxs <-
    extractFileNameIndexes
      [ gendir </> file
      | file <- files
      , takeExtension file == ".vkey"
      ]
  firstExceptT GenesisCmdTextEnvReadFileError $
    fromList
      <$> sequence
        [ (,) ix <$> readKey (File file)
        | (file, ix) <- fileIxs
        ]
 where
  readKey =
    newExceptT
      . readFileTextEnvelope (AsVerificationKey AsGenesisKey)

readDelegateKeys
  :: FilePath
  -> ExceptT
      GenesisCmdError
      IO
      (Map Int (VerificationKey GenesisDelegateKey))
readDelegateKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <-
    extractFileNameIndexes
      [ deldir </> file
      | file <- files
      , takeExtensions file == ".vkey"
      ]
  firstExceptT GenesisCmdTextEnvReadFileError $
    fromList
      <$> sequence
        [ (,) ix <$> readKey (File file)
        | (file, ix) <- fileIxs
        ]
 where
  readKey =
    newExceptT
      . readFileTextEnvelope (AsVerificationKey AsGenesisDelegateKey)

readDelegateVrfKeys
  :: FilePath
  -> ExceptT
      GenesisCmdError
      IO
      (Map Int (VerificationKey VrfKey))
readDelegateVrfKeys deldir = do
  files <- liftIO (listDirectory deldir)
  fileIxs <-
    extractFileNameIndexes
      [ deldir </> file
      | file <- files
      , takeExtensions file == ".vrf.vkey"
      ]
  firstExceptT GenesisCmdTextEnvReadFileError $
    fromList
      <$> sequence
        [ (,) ix <$> readKey (File file)
        | (file, ix) <- fileIxs
        ]
 where
  readKey =
    newExceptT
      . readFileTextEnvelope (AsVerificationKey AsVrfKey)

-- | The file path is of the form @"delegate-keys/delegate3.vkey"@.
-- This function reads the file and extracts the index (in this case 3).
extractFileNameIndex :: FilePath -> Maybe Int
extractFileNameIndex fp =
  case filter isDigit fp of
    [] -> Nothing
    xs -> readMaybe xs

extractFileNameIndexes
  :: [FilePath]
  -> ExceptT GenesisCmdError IO [(FilePath, Int)]
extractFileNameIndexes files = do
  case [file | (file, Nothing) <- filesIxs] of
    [] -> return ()
    files' -> throwError (GenesisCmdFilesNoIndex files')
  case filter (\g -> length g > 1)
    . List.groupBy ((==) `on` snd)
    . List.sortBy (compare `on` snd)
    $ [(file, ix) | (file, Just ix) <- filesIxs] of
    [] -> return ()
    (g : _) -> throwError (GenesisCmdFilesDupIndex (map fst g))

  return [(file, ix) | (file, Just ix) <- filesIxs]
 where
  filesIxs = [(file, extractFileNameIndex file) | file <- files]

readInitialFundAddresses
  :: FilePath
  -> NetworkId
  -> ExceptT GenesisCmdError IO [AddressInEra ShelleyEra]
readInitialFundAddresses utxodir nw = do
  files <- liftIO (listDirectory utxodir)
  vkeys <-
    firstExceptT GenesisCmdTextEnvReadFileError $
      sequence
        [ newExceptT $
            readFileTextEnvelope
              (AsVerificationKey AsGenesisUTxOKey)
              (File (utxodir </> file))
        | file <- files
        , takeExtension file == ".vkey"
        ]
  return
    [ addr
    | vkey <- vkeys
    , let vkh = verificationKeyHash (castVerificationKey vkey)
          addr =
            makeShelleyAddressInEra
              ShelleyBasedEraShelley
              nw
              (PaymentCredentialByKey vkh)
              NoStakeAddress
    ]

-- | Hash a genesis file
runGenesisHashFileCmd :: GenesisFile -> ExceptT GenesisCmdError IO ()
runGenesisHashFileCmd (GenesisFile fpath) = do
  content <-
    handleIOExceptT (GenesisCmdGenesisFileError . FileIOError fpath) $
      BS.readFile fpath
  let gh :: Crypto.Hash Crypto.Blake2b_256 ByteString
      gh = Crypto.hashWith id content
  liftIO $ Text.putStrLn (Crypto.hashToTextAsHex gh)
