{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Run.Genesis
  ( runGenesisCmds
  , runGenesisAddrCmd
  , runGenesisCreateCardanoCmd
  , runGenesisCreateCmd
  , runGenesisCreateStakedCmd
  , runGenesisHashFileCmd
  , runGenesisKeyGenDelegateCmd
  , runGenesisKeyGenGenesisCmd
  , runGenesisKeyGenUTxOCmd
  , runGenesisKeyHashCmd
  , runGenesisTxInCmd
  , runGenesisVerKeyCmd
  , readAndDecodeShelleyGenesis

    -- * Protocol Parameters
  , readProtocolParameters
  ) where

import Cardano.Api
import Cardano.Api.Byron
  ( toByronLovelace
  , toByronProtocolMagicId
  , toByronRequiresNetworkMagic
  )
import Cardano.Api.Shelley

import Cardano.CLI.Byron.Delegation
import Cardano.CLI.Byron.Genesis as Byron
import qualified Cardano.CLI.Byron.Key as Byron
import Cardano.CLI.EraBased.Commands.Genesis
import Cardano.CLI.EraBased.Run.Node
  ( runNodeIssueOpCertCmd
  , runNodeKeyGenColdCmd
  , runNodeKeyGenKesCmd
  , runNodeKeyGenVrfCmd
  )
import Cardano.CLI.EraBased.Run.StakeAddress (runStakeAddressKeyGenCmd)
import qualified Cardano.CLI.IO.Lazy as Lazy
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.GenesisCmdError
import Cardano.CLI.Types.Errors.NodeCmdError
import Cardano.CLI.Types.Errors.ProtocolParamsError
import Cardano.CLI.Types.Errors.StakePoolCmdError
import Cardano.CLI.Types.Key
import Cardano.Chain.Common (BlockCount (unBlockCount))
import qualified Cardano.Chain.Common as Byron (KeyHash, mkKnownLovelace, rationalToLovelacePortion)
import Cardano.Chain.Delegation (delegateVK)
import qualified Cardano.Chain.Delegation as Dlg
import Cardano.Chain.Genesis
  ( FakeAvvmOptions (..)
  , TestnetBalanceOptions (..)
  , gdProtocolParameters
  , gsDlgIssuersSecrets
  , gsPoorSecrets
  , gsRichSecrets
  )
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Update hiding (ProtocolParameters)
import qualified Cardano.Crypto as CC
import Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Binary (Annotated (Annotated), ToCBOR (..))
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Conway.Genesis as Conway
import Cardano.Ledger.Core (ppMinUTxOValueL)
import Cardano.Ledger.Crypto (ADDRHASH, Crypto, StandardCrypto)
import Cardano.Ledger.Era ()
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Shelley.Genesis (secondsToNominalDiffTimeMicro)
import Cardano.Prelude (canonicalEncodePretty)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesisStaking (..))

import Control.DeepSeq (NFData, force)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT, throwE, withExceptT)
import Control.Monad.Trans.Except.Extra
  ( firstExceptT
  , handleIOExceptT
  , hoistEither
  , left
  , newExceptT
  )
import Data.Aeson hiding (Key)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.KeyMap as Aeson
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Binary.Get as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Either (fromRight)
import Data.Fixed (Fixed (MkFixed))
import Data.Function (on)
import Data.Functor (void)
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.List.Split as List
import Data.ListMap (ListMap (..))
import qualified Data.ListMap as ListMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as Seq
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeExtension, takeExtensions, (</>))
import qualified System.IO as IO
import System.IO.Error (isDoesNotExistError)
import System.Random (StdGen)
import qualified System.Random as Random
import Text.JSON.Canonical (parseCanonicalJSON, renderCanonicalJSON)
import qualified Text.JSON.Canonical (ToJSON)
import Text.Read (readMaybe)

import Crypto.Random as Crypto

runGenesisCmds :: GenesisCmds era -> ExceptT GenesisCmdError IO ()
runGenesisCmds = \case
  GenesisKeyGenGenesis vk sk ->
    runGenesisKeyGenGenesisCmd vk sk
  GenesisKeyGenDelegate vk sk ctr ->
    runGenesisKeyGenDelegateCmd vk sk ctr
  GenesisKeyGenUTxO vk sk ->
    runGenesisKeyGenUTxOCmd vk sk
  GenesisCmdKeyHash vk ->
    runGenesisKeyHashCmd vk
  GenesisVerKey vk sk ->
    runGenesisVerKeyCmd vk sk
  GenesisTxIn vk nw mOutFile ->
    runGenesisTxInCmd vk nw mOutFile
  GenesisAddr vk nw mOutFile ->
    runGenesisAddrCmd vk nw mOutFile
  GenesisCreate fmt gd gn un ms am nw ->
    runGenesisCreateCmd fmt gd gn un ms am nw
  GenesisCreateCardano gd gn un ms am k slotLength sc nw bg sg ag cg mNodeCfg ->
    runGenesisCreateCardanoCmd gd gn un ms am k slotLength sc nw bg sg ag cg mNodeCfg
  GenesisCreateStaked fmt gd gn gp gl un ms am ds nw bf bp su relayJsonFp ->
    runGenesisCreateStakedCmd fmt gd gn gp gl un ms am ds nw bf bp su relayJsonFp
  GenesisHashFile gf ->
    runGenesisHashFileCmd gf

runGenesisKeyGenGenesisCmd
  :: VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenGenesisCmd vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsGenesisKey
  let vkey = getVerificationKey skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile skeyPath
    $ textEnvelopeToJSON (Just skeyDesc) skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile vkeyPath
    $ textEnvelopeToJSON (Just vkeyDesc) vkey
 where
  skeyDesc, vkeyDesc :: TextEnvelopeDescr
  skeyDesc = "Genesis Signing Key"
  vkeyDesc = "Genesis Verification Key"

runGenesisKeyGenDelegateCmd
  :: VerificationKeyFile Out
  -> SigningKeyFile Out
  -> OpCertCounterFile Out
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenDelegateCmd vkeyPath skeyPath ocertCtrPath = do
  skey <- liftIO $ generateSigningKey AsGenesisDelegateKey
  let vkey = getVerificationKey skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile skeyPath
    $ textEnvelopeToJSON (Just skeyDesc) skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile vkeyPath
    $ textEnvelopeToJSON (Just vkeyDesc) vkey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile ocertCtrPath
    $ textEnvelopeToJSON (Just certCtrDesc)
    $ OperationalCertificateIssueCounter
      initialCounter
      (castVerificationKey vkey) -- Cast to a 'StakePoolKey'
 where
  skeyDesc, vkeyDesc, certCtrDesc :: TextEnvelopeDescr
  skeyDesc = "Genesis delegate operator key"
  vkeyDesc = "Genesis delegate operator key"
  certCtrDesc =
    "Next certificate issue number: "
      <> fromString (show initialCounter)

  initialCounter :: Word64
  initialCounter = 0

runGenesisKeyGenDelegateVRF
  :: VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenDelegateVRF vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsVrfKey
  let vkey = getVerificationKey skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile skeyPath
    $ textEnvelopeToJSON (Just skeyDesc) skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile vkeyPath
    $ textEnvelopeToJSON (Just vkeyDesc) vkey
 where
  skeyDesc, vkeyDesc :: TextEnvelopeDescr
  skeyDesc = "VRF Signing Key"
  vkeyDesc = "VRF Verification Key"

runGenesisKeyGenUTxOCmd
  :: VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenUTxOCmd vkeyPath skeyPath = do
  skey <- liftIO $ generateSigningKey AsGenesisUTxOKey
  let vkey = getVerificationKey skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile skeyPath
    $ textEnvelopeToJSON (Just skeyDesc) skey
  firstExceptT GenesisCmdGenesisFileError
    . newExceptT
    $ writeLazyByteStringFile vkeyPath
    $ textEnvelopeToJSON (Just vkeyDesc) vkey
 where
  skeyDesc, vkeyDesc :: TextEnvelopeDescr
  skeyDesc = "Genesis Initial UTxO Signing Key"
  vkeyDesc = "Genesis Initial UTxO Verification Key"

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

  renderVerificationKeyHash :: (Key keyrole) => VerificationKey keyrole -> ByteString
  renderVerificationKeyHash =
    serialiseToRawBytesHex
      . verificationKeyHash

runGenesisVerKeyCmd
  :: VerificationKeyFile Out
  -> SigningKeyFile In
  -> ExceptT GenesisCmdError IO ()
runGenesisVerKeyCmd vkeyPath skeyPath = do
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
        skeyPath

  let vkey :: SomeGenesisKey VerificationKey
      vkey = case skey of
        AGenesisKey sk -> AGenesisKey (getVerificationKey sk)
        AGenesisDelegateKey sk -> AGenesisDelegateKey (getVerificationKey sk)
        AGenesisUTxOKey sk -> AGenesisUTxOKey (getVerificationKey sk)

  firstExceptT GenesisCmdGenesisFileError . newExceptT . liftIO $
    case vkey of
      AGenesisKey vk -> writeLazyByteStringFile vkeyPath $ textEnvelopeToJSON Nothing vk
      AGenesisDelegateKey vk -> writeLazyByteStringFile vkeyPath $ textEnvelopeToJSON Nothing vk
      AGenesisUTxOKey vk -> writeLazyByteStringFile vkeyPath $ textEnvelopeToJSON Nothing vk

data SomeGenesisKey f
  = AGenesisKey (f GenesisKey)
  | AGenesisDelegateKey (f GenesisDelegateKey)
  | AGenesisUTxOKey (f GenesisUTxOKey)

runGenesisTxInCmd
  :: VerificationKeyFile In
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT GenesisCmdError IO ()
runGenesisTxInCmd vkeyPath network mOutFile = do
  vkey <-
    firstExceptT GenesisCmdTextEnvReadFileError . newExceptT $
      readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
  let txin = genesisUTxOPseudoTxIn network (verificationKeyHash vkey)
  liftIO $ writeOutput mOutFile (renderTxIn txin)

runGenesisAddrCmd
  :: VerificationKeyFile In
  -> NetworkId
  -> Maybe (File () Out)
  -> ExceptT GenesisCmdError IO ()
runGenesisAddrCmd vkeyPath network mOutFile = do
  vkey <-
    firstExceptT GenesisCmdTextEnvReadFileError . newExceptT $
      readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey) vkeyPath
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
  :: KeyOutputFormat
  -> GenesisDir
  -> Word
  -- ^ num genesis & delegate keys to make
  -> Word
  -- ^ num utxo keys to make
  -> Maybe SystemStart
  -> Maybe Lovelace
  -> NetworkId
  -> ExceptT GenesisCmdError IO ()
runGenesisCreateCmd
  fmt
  (GenesisDir rootdir)
  genNumGenesisKeys
  genNumUTxOKeys
  mStart
  mAmount
  network = do
    liftIO $ do
      createDirectoryIfMissing False rootdir
      createDirectoryIfMissing False gendir
      createDirectoryIfMissing False deldir
      createDirectoryIfMissing False utxodir

    template <- readShelleyGenesisWithDefault (rootdir </> "genesis.spec.json") adjustTemplate
    alonzoGenesis <- readAlonzoGenesis (rootdir </> "genesis.alonzo.spec.json")
    conwayGenesis <- readConwayGenesis (rootdir </> "genesis.conway.spec.json")

    forM_ [1 .. genNumGenesisKeys] $ \index -> do
      createGenesisKeys gendir index
      createDelegateKeys fmt deldir index

    forM_ [1 .. genNumUTxOKeys] $ \index ->
      createUtxoKeys utxodir index

    genDlgs <- readGenDelegsMap gendir deldir
    utxoAddrs <- readInitialFundAddresses utxodir network
    start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

    let shelleyGenesis =
          updateTemplate
            -- Shelley genesis parameters
            start
            genDlgs
            mAmount
            utxoAddrs
            mempty
            (Lovelace 0)
            []
            []
            template

    void $ writeFileGenesis (rootdir </> "genesis.json") $ WritePretty shelleyGenesis
    void $ writeFileGenesis (rootdir </> "genesis.alonzo.json") $ WritePretty alonzoGenesis
    void $ writeFileGenesis (rootdir </> "genesis.conway.json") $ WritePretty conwayGenesis
   where
    -- TODO: rationalise the naming convention on these genesis json files.

    adjustTemplate t = t {sgNetworkMagic = unNetworkMagic (toNetworkMagic network)}
    gendir = rootdir </> "genesis-keys"
    deldir = rootdir </> "delegate-keys"
    utxodir = rootdir </> "utxo-keys"

toSKeyJSON :: (Key a) => SigningKey a -> ByteString
toSKeyJSON = LBS.toStrict . textEnvelopeToJSON Nothing

toVkeyJSON
  :: ()
  => (Key a)
  => (HasTypeProxy a)
  => SigningKey a
  -> ByteString
toVkeyJSON = LBS.toStrict . textEnvelopeToJSON Nothing . getVerificationKey

toVkeyJSON' :: (Key a) => VerificationKey a -> ByteString
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
    delegateMap = Map.fromList . map hashKeys $ combinedMap

  return (delegateMap, vrfKeys, kesKeys, opCerts)

--
-- Create Genesis Cardano command implementation
--

runGenesisCreateCardanoCmd
  :: GenesisDir
  -> Word
  -- ^ num genesis & delegate keys to make
  -> Word
  -- ^ num utxo keys to make
  -> Maybe SystemStart
  -> Maybe Lovelace
  -> BlockCount
  -> Word
  -- ^ slot length in ms
  -> Rational
  -> NetworkId
  -> FilePath
  -- ^ Byron Genesis
  -> FilePath
  -- ^ Shelley Genesis
  -> FilePath
  -- ^ Alonzo Genesis
  -> FilePath
  -- ^ Conway Genesis
  -> Maybe FilePath
  -> ExceptT GenesisCmdError IO ()
runGenesisCreateCardanoCmd
  (GenesisDir rootdir)
  genNumGenesisKeys
  genNumUTxOKeys
  mStart
  mAmount
  mSecurity
  slotLength
  mSlotCoeff
  network
  byronGenesisT
  shelleyGenesisT
  alonzoGenesisT
  conwayGenesisT
  mNodeCfg = do
    start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart
    (byronGenesis', byronSecrets) <- convertToShelleyError $ Byron.mkGenesis $ byronParams start
    let
      byronGenesis =
        byronGenesis'
          { gdProtocolParameters =
              (gdProtocolParameters byronGenesis')
                { ppSlotDuration = floor (toRational slotLength * recip mSlotCoeff)
                }
          }

      genesisKeys = gsDlgIssuersSecrets byronSecrets
      byronGenesisKeys = map ByronSigningKey genesisKeys
      shelleyGenesisKeys = map convertGenesisKey genesisKeys
      shelleyGenesisvkeys :: [VerificationKey GenesisKey]
      shelleyGenesisvkeys = map (castVerificationKey . getVerificationKey) shelleyGenesisKeys

      delegateKeys = gsRichSecrets byronSecrets
      byronDelegateKeys = map ByronSigningKey delegateKeys
      shelleyDelegateKeys :: [SigningKey GenesisDelegateExtendedKey]
      shelleyDelegateKeys = map convertDelegate delegateKeys
      shelleyDelegatevkeys :: [VerificationKey GenesisDelegateKey]
      shelleyDelegatevkeys = map (castVerificationKey . getVerificationKey) shelleyDelegateKeys

      utxoKeys = gsPoorSecrets byronSecrets
      byronUtxoKeys = map (ByronSigningKey . Genesis.poorSecretToKey) utxoKeys
      shelleyUtxoKeys = map (convertPoor . Genesis.poorSecretToKey) utxoKeys

    dlgCerts <- convertToShelleyError $ mapM (findDelegateCert byronGenesis) byronDelegateKeys
    let
      overrideShelleyGenesis t =
        t
          { sgNetworkMagic = unNetworkMagic (toNetworkMagic network)
          , sgNetworkId = toShelleyNetwork network
          , sgActiveSlotsCoeff =
              fromMaybe (error $ "Could not convert from Rational: " ++ show mSlotCoeff) $
                Ledger.boundRational mSlotCoeff
          , sgSecurityParam = unBlockCount mSecurity
          , sgUpdateQuorum = fromIntegral $ ((genNumGenesisKeys `div` 3) * 2) + 1
          , sgEpochLength = EpochSize $ floor $ (fromIntegral (unBlockCount mSecurity) * 10) / mSlotCoeff
          , sgMaxLovelaceSupply = 45000000000000000
          , sgSystemStart = getSystemStart start
          , sgSlotLength = secondsToNominalDiffTimeMicro $ MkFixed (fromIntegral slotLength) * 1000
          }
    shelleyGenesisTemplate <-
      liftIO $
        overrideShelleyGenesis . fromRight (error "shelley genesis template not found")
          <$> readAndDecodeShelleyGenesis shelleyGenesisT
    alonzoGenesis <- readAlonzoGenesis alonzoGenesisT
    conwayGenesis <- readConwayGenesis conwayGenesisT
    (delegateMap, vrfKeys, kesKeys, opCerts) <-
      liftIO $ generateShelleyNodeSecrets shelleyDelegateKeys shelleyGenesisvkeys
    let
      shelleyGenesis :: ShelleyGenesis StandardCrypto
      shelleyGenesis = updateTemplate start delegateMap Nothing [] mempty 0 [] [] shelleyGenesisTemplate

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
      writeFileGenesis (rootdir </> "byron-genesis.json") $ WriteCanonical byronGenesis
    shelleyGenesisHash <-
      writeFileGenesis (rootdir </> "shelley-genesis.json") $ WritePretty shelleyGenesis
    alonzoGenesisHash <-
      writeFileGenesis (rootdir </> "alonzo-genesis.json") $ WritePretty alonzoGenesis
    conwayGenesisHash <-
      writeFileGenesis (rootdir </> "conway-genesis.json") $ WritePretty conwayGenesis

    liftIO $ do
      case mNodeCfg of
        Nothing -> pure ()
        Just nodeCfg -> do
          nodeConfig <- Yaml.decodeFileThrow nodeCfg
          let
            setHash field hash = Aeson.insert field $ String $ Crypto.hashToTextAsHex hash
            updateConfig :: Yaml.Value -> Yaml.Value
            updateConfig (Object obj) =
              Object $
                setHash "ByronGenesisHash" byronGenesisHash $
                  setHash "ShelleyGenesisHash" shelleyGenesisHash $
                    setHash "AlonzoGenesisHash" alonzoGenesisHash $
                      setHash
                        "ConwayGenesisHash"
                        conwayGenesisHash
                        obj
            updateConfig x = x
            newConfig :: Yaml.Value
            newConfig = updateConfig nodeConfig
          encodeFile (rootdir </> "node-config.json") newConfig
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
        byronGenesisT
        mSecurity
        byronNetwork
        byronBalance
        byronFakeAvvm
        byronAvvmFactor
        Nothing
    gendir = rootdir </> "genesis-keys"
    deldir = rootdir </> "delegate-keys"
    utxodir = rootdir </> "utxo-keys"
    byronNetwork =
      CC.AProtocolMagic
        (Annotated (toByronProtocolMagicId network) ())
        (toByronRequiresNetworkMagic network)
    byronBalance =
      TestnetBalanceOptions
        { tboRichmen = genNumGenesisKeys
        , tboPoors = genNumUTxOKeys
        , tboTotalBalance = fromMaybe zeroLovelace $ toByronLovelace (fromMaybe 0 mAmount)
        , tboRichmenShare = 0
        }
    byronFakeAvvm =
      FakeAvvmOptions
        { faoCount = 0
        , faoOneBalance = zeroLovelace
        }
    byronAvvmFactor = Byron.rationalToLovelacePortion 0.0
    zeroLovelace = Byron.mkKnownLovelace @0

    -- Compare a given 'SigningKey' with a 'Certificate' 'VerificationKey'
    isCertForSK :: CC.SigningKey -> Dlg.Certificate -> Bool
    isCertForSK sk cert = delegateVK cert == CC.toVerification sk

    findDelegateCert
      :: Genesis.GenesisData -> SigningKey ByronKey -> ExceptT ByronGenesisError IO Dlg.Certificate
    findDelegateCert byronGenesis bSkey@(ByronSigningKey sk) = do
      case List.find (isCertForSK sk) (Map.elems $ dlgCertMap byronGenesis) of
        Nothing ->
          throwE
            . NoGenesisDelegationForKey
            . Byron.prettyPublicKey
            $ getVerificationKey bSkey
        Just x -> pure x

    dlgCertMap :: Genesis.GenesisData -> Map Byron.KeyHash Dlg.Certificate
    dlgCertMap byronGenesis = Genesis.unGenesisDelegation $ Genesis.gdHeavyDelegation byronGenesis

runGenesisCreateStakedCmd
  :: KeyOutputFormat
  -- ^ key output format
  -> GenesisDir
  -> Word
  -- ^ num genesis & delegate keys to make
  -> Word
  -- ^ num utxo keys to make
  -> Word
  -- ^ num pools to make
  -> Word
  -- ^ num delegators to make
  -> Maybe SystemStart
  -> Maybe Lovelace
  -- ^ supply going to non-delegators
  -> Lovelace
  -- ^ supply going to delegators
  -> NetworkId
  -> Word
  -- ^ bulk credential files to write
  -> Word
  -- ^ pool credentials per bulk file
  -> Word
  -- ^ num stuffed UTxO entries
  -> Maybe FilePath
  -- ^ Specified stake pool relays
  -> ExceptT GenesisCmdError IO ()
runGenesisCreateStakedCmd
  fmt
  (GenesisDir rootdir)
  genNumGenesisKeys
  genNumUTxOKeys
  genNumPools
  genNumStDelegs
  mStart
  mNonDlgAmount
  stDlgAmount
  network
  numBulkPoolCredFiles
  bulkPoolsPerFile
  numStuffedUtxo
  sPoolRelayFp = do
    liftIO $ do
      createDirectoryIfMissing False rootdir
      createDirectoryIfMissing False gendir
      createDirectoryIfMissing False deldir
      createDirectoryIfMissing False pooldir
      createDirectoryIfMissing False stdeldir
      createDirectoryIfMissing False utxodir

    template <- readShelleyGenesisWithDefault (rootdir </> "genesis.spec.json") adjustTemplate
    alonzoGenesis <- readAlonzoGenesis (rootdir </> "genesis.alonzo.spec.json")
    conwayGenesis <- readConwayGenesis (rootdir </> "genesis.conway.spec.json")

    forM_ [1 .. genNumGenesisKeys] $ \index -> do
      createGenesisKeys gendir index
      createDelegateKeys fmt deldir index

    forM_ [1 .. genNumUTxOKeys] $ \index ->
      createUtxoKeys utxodir index

    mayStakePoolRelays <-
      forM sPoolRelayFp $
        \fp -> do
          relaySpecJsonBs <-
            handleIOExceptT (GenesisCmdStakePoolRelayFileError fp) (LBS.readFile fp)
          firstExceptT (GenesisCmdStakePoolRelayJsonDecodeError fp)
            . hoistEither
            $ Aeson.eitherDecode relaySpecJsonBs

    poolParams <- forM [1 .. genNumPools] $ \index -> do
      createPoolCredentials fmt pooldir index
      buildPoolParams network pooldir index (fromMaybe mempty mayStakePoolRelays)

    when (numBulkPoolCredFiles * bulkPoolsPerFile > genNumPools) $
      left $
        GenesisCmdTooFewPoolsForBulkCreds genNumPools numBulkPoolCredFiles bulkPoolsPerFile
    -- We generate the bulk files for the last pool indices,
    -- so that all the non-bulk pools have stable indices at beginning:
    let bulkOffset = fromIntegral $ genNumPools - numBulkPoolCredFiles * bulkPoolsPerFile
        bulkIndices :: [Word] = [1 + bulkOffset .. genNumPools]
        bulkSlices :: [[Word]] = List.chunksOf (fromIntegral bulkPoolsPerFile) bulkIndices
    forM_ (zip [1 .. numBulkPoolCredFiles] bulkSlices) $
      uncurry (writeBulkPoolCredentials pooldir)

    let (delegsPerPool, delegsRemaining) = divMod genNumStDelegs genNumPools
        delegsForPool poolIx =
          if delegsRemaining /= 0 && poolIx == genNumPools
            then delegsPerPool
            else delegsPerPool + delegsRemaining
        distribution = [pool | (pool, poolIx) <- zip poolParams [1 ..], _ <- [1 .. delegsForPool poolIx]]

    g <- Random.getStdGen

    -- Distribute M delegates across N pools:
    delegations <- liftIO $ Lazy.forStateM g distribution $ flip computeInsecureDelegation network

    let numDelegations = length delegations

    genDlgs <- readGenDelegsMap gendir deldir
    nonDelegAddrs <- readInitialFundAddresses utxodir network
    start <- maybe (SystemStart <$> getCurrentTimePlus30) pure mStart

    stuffedUtxoAddrs <- liftIO $ Lazy.replicateM (fromIntegral numStuffedUtxo) genStuffedAddress

    let stake = second Ledger.ppId . mkDelegationMapEntry <$> delegations
        stakePools =
          [ (Ledger.ppId poolParams', poolParams') | poolParams' <- snd . mkDelegationMapEntry <$> delegations
          ]
        delegAddrs = dInitialUtxoAddr <$> delegations
        !shelleyGenesis =
          updateCreateStakedOutputTemplate
            -- Shelley genesis parameters
            start
            genDlgs
            mNonDlgAmount
            (length nonDelegAddrs)
            nonDelegAddrs
            stakePools
            stake
            stDlgAmount
            numDelegations
            delegAddrs
            stuffedUtxoAddrs
            template

    liftIO $ LBS.writeFile (rootdir </> "genesis.json") $ Aeson.encode shelleyGenesis

    void $ writeFileGenesis (rootdir </> "genesis.alonzo.json") $ WritePretty alonzoGenesis
    void $ writeFileGenesis (rootdir </> "genesis.conway.json") $ WritePretty conwayGenesis
    -- TODO: rationalise the naming convention on these genesis json files.

    liftIO $
      Text.hPutStrLn IO.stderr $
        mconcat $
          [ "generated genesis with: "
          , textShow genNumGenesisKeys
          , " genesis keys, "
          , textShow genNumUTxOKeys
          , " non-delegating UTxO keys, "
          , textShow genNumPools
          , " stake pools, "
          , textShow genNumStDelegs
          , " delegating UTxO keys, "
          , textShow numDelegations
          , " delegation map entries, "
          ]
            ++ [ mconcat
                [ ", "
                , textShow numBulkPoolCredFiles
                , " bulk pool credential files, "
                , textShow bulkPoolsPerFile
                , " pools per bulk credential file, indices starting from "
                , textShow bulkOffset
                , ", "
                , textShow $ length bulkIndices
                , " total pools in bulk nodes, each bulk node having this many entries: "
                , textShow $ length <$> bulkSlices
                ]
               | numBulkPoolCredFiles * bulkPoolsPerFile > 0
               ]
   where
    adjustTemplate t = t {sgNetworkMagic = unNetworkMagic (toNetworkMagic network)}
    mkDelegationMapEntry
      :: Delegation -> (Ledger.KeyHash Ledger.Staking StandardCrypto, Ledger.PoolParams StandardCrypto)
    mkDelegationMapEntry d = (dDelegStaking d, dPoolParams d)

    gendir = rootdir </> "genesis-keys"
    deldir = rootdir </> "delegate-keys"
    pooldir = rootdir </> "pools"
    stdeldir = rootdir </> "stake-delegator-keys"
    utxodir = rootdir </> "utxo-keys"

    genStuffedAddress :: IO (AddressInEra ShelleyEra)
    genStuffedAddress =
      shelleyAddressInEra
        <$> ( ShelleyAddress
                <$> pure Ledger.Testnet
                <*> ( Ledger.KeyHashObj . mkKeyHash . read64BitInt
                        <$> Crypto.runSecureRandom (getRandomBytes 8)
                    )
                <*> pure Ledger.StakeRefNull
            )

    read64BitInt :: ByteString -> Int
    read64BitInt =
      (fromIntegral :: Word64 -> Int)
        . Bin.runGet Bin.getWord64le
        . LBS.fromStrict

    mkDummyHash :: forall h a. (HashAlgorithm h) => Proxy h -> Int -> Hash.Hash h a
    mkDummyHash _ = coerce . Ledger.hashWithSerialiser @h toCBOR

    mkKeyHash :: forall c discriminator. (Crypto c) => Int -> Ledger.KeyHash discriminator c
    mkKeyHash = Ledger.KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

-- -------------------------------------------------------------------------------------------------

createDelegateKeys :: KeyOutputFormat -> FilePath -> Word -> ExceptT GenesisCmdError IO ()
createDelegateKeys fmt dir index = do
  liftIO $ createDirectoryIfMissing False dir
  runGenesisKeyGenDelegateCmd
    (File @(VerificationKey ()) $ dir </> "delegate" ++ strIndex ++ ".vkey")
    (onlyOut coldSK)
    (onlyOut opCertCtr)
  runGenesisKeyGenDelegateVRF
    (File @(VerificationKey ()) $ dir </> "delegate" ++ strIndex ++ ".vrf.vkey")
    (File @(SigningKey ()) $ dir </> "delegate" ++ strIndex ++ ".vrf.skey")
  firstExceptT GenesisCmdNodeCmdError $ do
    runNodeKeyGenKesCmd
      fmt
      (onlyOut kesVK)
      (File @(SigningKey ()) $ dir </> "delegate" ++ strIndex ++ ".kes.skey")
    runNodeIssueOpCertCmd
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
  runGenesisKeyGenGenesisCmd
    (File @(VerificationKey ()) $ dir </> "genesis" ++ strIndex ++ ".vkey")
    (File @(SigningKey ()) $ dir </> "genesis" ++ strIndex ++ ".skey")

createUtxoKeys :: FilePath -> Word -> ExceptT GenesisCmdError IO ()
createUtxoKeys dir index = do
  liftIO $ createDirectoryIfMissing False dir
  let strIndex = show index
  runGenesisKeyGenUTxOCmd
    (File @(VerificationKey ()) $ dir </> "utxo" ++ strIndex ++ ".vkey")
    (File @(SigningKey ()) $ dir </> "utxo" ++ strIndex ++ ".skey")

createPoolCredentials :: KeyOutputFormat -> FilePath -> Word -> ExceptT GenesisCmdError IO ()
createPoolCredentials fmt dir index = do
  liftIO $ createDirectoryIfMissing False dir
  firstExceptT GenesisCmdNodeCmdError $ do
    runNodeKeyGenKesCmd
      fmt
      (onlyOut kesVK)
      (File @(SigningKey ()) $ dir </> "kes" ++ strIndex ++ ".skey")
    runNodeKeyGenVrfCmd
      fmt
      (File @(VerificationKey ()) $ dir </> "vrf" ++ strIndex ++ ".vkey")
      (File @(SigningKey ()) $ dir </> "vrf" ++ strIndex ++ ".skey")
    runNodeKeyGenColdCmd
      fmt
      (File @(VerificationKey ()) $ dir </> "cold" ++ strIndex ++ ".vkey")
      (onlyOut coldSK)
      (onlyOut opCertCtr)
    runNodeIssueOpCertCmd
      (VerificationKeyFilePath (onlyIn kesVK))
      (onlyIn coldSK)
      opCertCtr
      (KESPeriod 0)
      (File $ dir </> "opcert" ++ strIndex ++ ".cert")
  firstExceptT GenesisCmdStakeAddressCmdError $
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
  , dDelegStaking :: !(Ledger.KeyHash Ledger.Staking StandardCrypto)
  , dPoolParams :: !(Ledger.PoolParams StandardCrypto)
  }
  deriving (Generic, NFData)

buildPoolParams
  :: NetworkId
  -> FilePath
  -- ^ File directory where the necessary pool credentials were created
  -> Word
  -> Map Word [Ledger.StakePoolRelay]
  -- ^ User submitted stake pool relay map
  -> ExceptT GenesisCmdError IO (Ledger.PoolParams StandardCrypto)
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
    Ledger.PoolParams
      { Ledger.ppId = Ledger.hashKey poolColdVK
      , Ledger.ppVrf = Ledger.hashVerKeyVRF poolVrfVK
      , Ledger.ppPledge = Ledger.Coin 0
      , Ledger.ppCost = Ledger.Coin 0
      , Ledger.ppMargin = minBound
      , Ledger.ppRewardAcnt =
          toShelleyStakeAddr $ makeStakeAddress nw $ StakeCredentialByKey (verificationKeyHash rewardsSVK)
      , Ledger.ppOwners = mempty
      , Ledger.ppRelays = lookupPoolRelay specifiedRelays
      , Ledger.ppMetadata = Ledger.SNothing
      }
 where
  lookupPoolRelay
    :: Map Word [Ledger.StakePoolRelay] -> Seq.StrictSeq Ledger.StakePoolRelay
  lookupPoolRelay m = maybe mempty Seq.fromList (Map.lookup index m)

  strIndex = show index
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
    firstExceptT (GenesisCmdAesonDecodeError fp . Text.pack) . hoistEither $
      Aeson.eitherDecodeStrict' content

-- | This function should only be used for testing purposes.
-- Keys returned by this function are not cryptographically secure.
computeInsecureDelegation
  :: StdGen
  -> NetworkId
  -> Ledger.PoolParams StandardCrypto
  -> IO (StdGen, Delegation)
computeInsecureDelegation g0 nw pool = do
  (paymentVK, g1) <- first getVerificationKey <$> generateInsecureSigningKey g0 AsPaymentKey
  (stakeVK, g2) <- first getVerificationKey <$> generateInsecureSigningKey g1 AsStakeKey

  let stakeAddressReference = StakeAddressByValue . StakeCredentialByKey . verificationKeyHash $ stakeVK
  let initialUtxoAddr =
        makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash paymentVK)) stakeAddressReference

  delegation <-
    pure $
      force
        Delegation
          { dInitialUtxoAddr = shelleyAddressInEra initialUtxoAddr
          , dDelegStaking = Ledger.hashKey (unStakeVerificationKey stakeVK)
          , dPoolParams = pool
          }

  pure (g2, delegation)

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT a IO UTCTime
getCurrentTimePlus30 =
  plus30sec <$> liftIO getCurrentTime
 where
  plus30sec :: UTCTime -> UTCTime
  plus30sec = addUTCTime (30 :: NominalDiffTime)

-- | Attempts to read Shelley genesis from disk
-- and if not found creates a default Shelley genesis.
readShelleyGenesisWithDefault
  :: FilePath
  -> (ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto)
  -> ExceptT GenesisCmdError IO (ShelleyGenesis StandardCrypto)
readShelleyGenesisWithDefault fpath adjustDefaults = do
  newExceptT (readAndDecodeShelleyGenesis fpath)
    `catchError` \err ->
      case err of
        GenesisCmdGenesisFileReadError (FileIOError _ ioe)
          | isDoesNotExistError ioe -> writeDefault
        _ -> left err
 where
  defaults :: ShelleyGenesis StandardCrypto
  defaults = adjustDefaults shelleyGenesisDefaults

  writeDefault = do
    handleIOExceptT (GenesisCmdGenesisFileError . FileIOError fpath) $
      LBS.writeFile fpath (encode defaults)
    return defaults

readAndDecodeShelleyGenesis
  :: FilePath
  -> IO (Either GenesisCmdError (ShelleyGenesis StandardCrypto))
readAndDecodeShelleyGenesis fpath = runExceptT $ do
  lbs <- handleIOExceptT (GenesisCmdGenesisFileReadError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (GenesisCmdGenesisFileDecodeError fpath . Text.pack)
    . hoistEither
    $ Aeson.eitherDecode' lbs

updateTemplate
  :: SystemStart
  -- ^ System start time
  -> Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey)
  -- ^ Genesis delegation (not stake-based)
  -> Maybe Lovelace
  -- ^ Amount of lovelace not delegated
  -> [AddressInEra ShelleyEra]
  -- ^ UTxO addresses that are not delegating
  -> Map (Ledger.KeyHash 'Ledger.Staking StandardCrypto) (Ledger.PoolParams StandardCrypto)
  -- ^ Genesis staking: pools/delegation map & delegated initial UTxO spec
  -> Lovelace
  -- ^ Number of UTxO Addresses for delegation
  -> [AddressInEra ShelleyEra]
  -- ^ UTxO Addresses for delegation
  -> [AddressInEra ShelleyEra]
  -- ^ Stuffed UTxO addresses
  -> ShelleyGenesis StandardCrypto
  -- ^ Template from which to build a genesis
  -> ShelleyGenesis StandardCrypto
  -- ^ Updated genesis
updateTemplate
  (SystemStart start)
  genDelegMap
  mAmountNonDeleg
  utxoAddrsNonDeleg
  poolSpecs
  (Lovelace amountDeleg)
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
                ListMap.fromList
                  [ (toShelleyAddr addr, toShelleyLovelace v)
                  | (addr, v) <-
                      distribute (nonDelegCoin - subtractForTreasury) utxoAddrsNonDeleg
                        ++ distribute (delegCoin - subtractForTreasury) utxoAddrsDeleg
                        ++ mkStuffedUtxo stuffedUtxoAddrs
                  ]
            , sgStaking =
                ShelleyGenesisStaking
                  { sgsPools =
                      ListMap.fromList
                        [ (Ledger.ppId poolParams, poolParams)
                        | poolParams <- Map.elems poolSpecs
                        ]
                  , sgsStake = ListMap.fromMap $ Ledger.ppId <$> poolSpecs
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
            ((addr, Lovelace coinPerAddr) : acc, rest - coinPerAddr)
        | otherwise = ((addr, Lovelace rest) : acc, 0)

    mkStuffedUtxo :: [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    mkStuffedUtxo xs = (,Lovelace minUtxoVal) <$> xs
     where
      Coin minUtxoVal = sgProtocolParams template ^. ppMinUTxOValueL

    shelleyDelKeys =
      Map.fromList
        [ (gh, Ledger.GenDelegPair gdh h)
        | ( GenesisKeyHash gh
            , (GenesisDelegateKeyHash gdh, VrfKeyHash h)
            ) <-
            Map.toList genDelegMap
        ]

    unLovelace :: (Integral a) => Lovelace -> a
    unLovelace (Lovelace coin) = fromIntegral coin

updateCreateStakedOutputTemplate
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
  -> [(Ledger.KeyHash 'Ledger.StakePool StandardCrypto, Ledger.PoolParams StandardCrypto)]
  -- ^ Pool map
  -> [(Ledger.KeyHash 'Ledger.Staking StandardCrypto, Ledger.KeyHash 'Ledger.StakePool StandardCrypto)]
  -- ^ Delegaton map
  -> Lovelace
  -- ^ Amount of lovelace to delegate
  -> Int
  -- ^ Number of UTxO address for delegationg
  -> [AddressInEra ShelleyEra]
  -- ^ UTxO address for delegationg
  -> [AddressInEra ShelleyEra]
  -- ^ Stuffed UTxO addresses
  -> ShelleyGenesis StandardCrypto
  -- ^ Template from which to build a genesis
  -> ShelleyGenesis StandardCrypto
  -- ^ Updated genesis
updateCreateStakedOutputTemplate
  (SystemStart start)
  genDelegMap
  mAmountNonDeleg
  nUtxoAddrsNonDeleg
  utxoAddrsNonDeleg
  pools
  stake
  (Lovelace amountDeleg)
  nUtxoAddrsDeleg
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
                ListMap.fromList
                  [ (toShelleyAddr addr, toShelleyLovelace v)
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

    distribute :: Integer -> Int -> [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    distribute funds nAddrs addrs = zip addrs (fmap Lovelace (coinPerAddr + remainder : repeat coinPerAddr))
     where
      coinPerAddr, remainder :: Integer
      (,) coinPerAddr remainder = funds `divMod` fromIntegral nAddrs

    mkStuffedUtxo :: [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    mkStuffedUtxo xs = (,Lovelace minUtxoVal) <$> xs
     where
      Coin minUtxoVal = sgProtocolParams template ^. ppMinUTxOValueL

    shelleyDelKeys =
      Map.fromList
        [ (gh, Ledger.GenDelegPair gdh h)
        | ( GenesisKeyHash gh
            , (GenesisDelegateKeyHash gdh, VrfKeyHash h)
            ) <-
            Map.toList genDelegMap
        ]

    unLovelace :: (Integral a) => Lovelace -> a
    unLovelace (Lovelace coin) = fromIntegral coin

writeFileGenesis
  :: FilePath
  -> WriteFileGenesis
  -> ExceptT GenesisCmdError IO (Crypto.Hash Crypto.Blake2b_256 ByteString)
writeFileGenesis fpath genesis = do
  handleIOExceptT (GenesisCmdGenesisFileError . FileIOError fpath) $
    BS.writeFile fpath content
  return $ Crypto.hashWith id content
 where
  content = case genesis of
    WritePretty a -> LBS.toStrict $ encodePretty a
    WriteCanonical a ->
      LBS.toStrict
        . renderCanonicalJSON
        . either (error "error parsing json that was just encoded!?") id
        . parseCanonicalJSON
        . canonicalEncodePretty
        $ a

data WriteFileGenesis where
  WriteCanonical :: (Text.JSON.Canonical.ToJSON Identity genesis) => genesis -> WriteFileGenesis
  WritePretty :: (ToJSON genesis) => genesis -> WriteFileGenesis

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
        Map.fromList
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
    Map.fromList
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
    Map.fromList
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
    Map.fromList
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
    [ addr | vkey <- vkeys, let vkh = verificationKeyHash (castVerificationKey vkey)
                                addr =
                                  makeShelleyAddressInEra
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

readAlonzoGenesis
  :: FilePath
  -> ExceptT GenesisCmdError IO Alonzo.AlonzoGenesis
readAlonzoGenesis fpath = do
  lbs <- handleIOExceptT (GenesisCmdGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (GenesisCmdAesonDecodeError fpath . Text.pack)
    . hoistEither
    $ Aeson.eitherDecode' lbs

readConwayGenesis
  :: FilePath
  -> ExceptT GenesisCmdError IO (Conway.ConwayGenesis StandardCrypto)
readConwayGenesis fpath = do
  lbs <- handleIOExceptT (GenesisCmdGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (GenesisCmdAesonDecodeError fpath . Text.pack)
    . hoistEither
    $ Aeson.eitherDecode' lbs

-- Protocol Parameters

-- TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters
  :: ProtocolParamsFile
  -> ExceptT ProtocolParamsError IO ProtocolParameters
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ProtocolParamsErrorFile . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ProtocolParamsErrorJSON fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams
