{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Run.CreateTestnetData
  ( genStuffedAddress
    , getCurrentTimePlus30
    , readAndDecodeShelleyGenesis
    , runGenesisKeyGenUTxOCmd
    , runGenesisKeyGenGenesisCmd
    , runGenesisKeyGenDelegateCmd
    , runGenesisCreateTestNetDataCmd
    , runGenesisKeyGenDelegateVRF
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.EraBased.Commands.Genesis as Cmd
import qualified Cardano.CLI.EraBased.Commands.Governance.DRep as DRep
import qualified Cardano.CLI.EraBased.Commands.Node as Cmd
import           Cardano.CLI.EraBased.Run.Address (runAddressKeyGenCmd)
import qualified Cardano.CLI.EraBased.Run.Governance.DRep as DRep
import qualified Cardano.CLI.EraBased.Run.Key as Key
import           Cardano.CLI.EraBased.Run.Node (runNodeIssueOpCertCmd, runNodeKeyGenColdCmd,
                   runNodeKeyGenKesCmd, runNodeKeyGenVrfCmd)
import           Cardano.CLI.EraBased.Run.StakeAddress (runStakeAddressKeyGenCmd)
import qualified Cardano.CLI.IO.Lazy as Lazy
import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GenesisCmdError
import           Cardano.CLI.Types.Errors.NodeCmdError
import           Cardano.CLI.Types.Errors.StakePoolCmdError
import           Cardano.CLI.Types.Key
import qualified Cardano.CLI.Types.Key as Keys
import           Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Binary (ToCBOR (..))
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Core (ppMinUTxOValueL)
import           Cardano.Ledger.Crypto (ADDRHASH, Crypto, StandardCrypto)
import           Cardano.Ledger.Era ()
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesisStaking (..))

import           Control.DeepSeq (NFData, force)
import           Control.Monad (forM, forM_, unless, void, when, zipWithM)
import           Control.Monad.Except (MonadError (..), runExceptT)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   newExceptT)
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.Binary.Get as Bin
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import           Data.Data (Proxy (..))
import           Data.ListMap (ListMap (..))
import qualified Data.ListMap as ListMap
import           Data.Map.Strict (Map, fromList, toList)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as Seq
import           Data.String (fromString)
import qualified Data.Text as Text
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Lens.Micro ((^.))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import qualified System.Random as Random
import           System.Random (StdGen)

import           Crypto.Random (getRandomBytes)


runGenesisKeyGenGenesisCmd
  :: GenesisKeyGenGenesisCmdArgs
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenGenesisCmd
    Cmd.GenesisKeyGenGenesisCmdArgs
    { Cmd.verificationKeyPath
    , Cmd.signingKeyPath
    } = do
    skey <- liftIO $ generateSigningKey AsGenesisKey
    let vkey = getVerificationKey skey
    firstExceptT GenesisCmdGenesisFileError . newExceptT $ do
      void $ writeLazyByteStringFile signingKeyPath $ textEnvelopeToJSON (Just skeyDesc) skey
      writeLazyByteStringFile verificationKeyPath $ textEnvelopeToJSON (Just Key.genesisVkeyDesc) vkey
  where
    skeyDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis Signing Key"


runGenesisKeyGenDelegateCmd
  :: GenesisKeyGenDelegateCmdArgs
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenDelegateCmd
    Cmd.GenesisKeyGenDelegateCmdArgs
    { Cmd.verificationKeyPath
    , Cmd.signingKeyPath
    , Cmd.opCertCounterPath
    } = do
    skey <- liftIO $ generateSigningKey AsGenesisDelegateKey
    let vkey = getVerificationKey skey
    firstExceptT GenesisCmdGenesisFileError . newExceptT $ do
      void $ writeLazyByteStringFile signingKeyPath
        $ textEnvelopeToJSON (Just skeyDesc) skey
      void $ writeLazyByteStringFile verificationKeyPath
        $ textEnvelopeToJSON (Just Key.genesisVkeyDelegateDesc) vkey
      writeLazyByteStringFile opCertCounterPath
        $ textEnvelopeToJSON (Just certCtrDesc)
        $ OperationalCertificateIssueCounter
            initialCounter
            (castVerificationKey vkey)  -- Cast to a 'StakePoolKey'
  where
    skeyDesc, certCtrDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis delegate operator key"
    certCtrDesc = "Next certificate issue number: "
               <> fromString (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runGenesisKeyGenDelegateVRF ::
     VerificationKeyFile Out
  -> SigningKeyFile Out
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenDelegateVRF vkeyPath skeyPath = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT GenesisCmdGenesisFileError . newExceptT $ do
      void $ writeLazyByteStringFile skeyPath
        $ textEnvelopeToJSON (Just skeyDesc) skey
      writeLazyByteStringFile vkeyPath
        $ textEnvelopeToJSON (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "VRF Signing Key"
    vkeyDesc = "VRF Verification Key"


runGenesisKeyGenUTxOCmd
  :: GenesisKeyGenUTxOCmdArgs
  -> ExceptT GenesisCmdError IO ()
runGenesisKeyGenUTxOCmd
    Cmd.GenesisKeyGenUTxOCmdArgs
    { Cmd.verificationKeyPath
    , Cmd.signingKeyPath
    } = do
    skey <- liftIO $ generateSigningKey AsGenesisUTxOKey
    let vkey = getVerificationKey skey
    firstExceptT GenesisCmdGenesisFileError . newExceptT $ do
      void $ writeLazyByteStringFile signingKeyPath
        $ textEnvelopeToJSON (Just skeyDesc) skey
      writeLazyByteStringFile verificationKeyPath
        $ textEnvelopeToJSON (Just vkeyDesc) vkey
  where
    skeyDesc, vkeyDesc :: TextEnvelopeDescr
    skeyDesc = "Genesis Initial UTxO Signing Key"
    vkeyDesc = "Genesis Initial UTxO Verification Key"


runGenesisCreateTestNetDataCmd
  :: GenesisCreateTestNetDataCmdArgs
  -> ExceptT GenesisCmdError IO ()
runGenesisCreateTestNetDataCmd Cmd.GenesisCreateTestNetDataCmdArgs
  {  networkId
   , specShelley
   , numGenesisKeys
   , numPools
   , stakeDelegators
   , numDrepKeys
   , numStuffedUtxo
   , numUtxoKeys
   , totalSupply
   , delegatedSupply
   , systemStart
   , outputDir }
   = do
  liftIO $ createDirectoryIfMissing False outputDir
  shelleyGenesis <-
    case specShelley of
      Just shelleyPath ->
        newExceptT $ readAndDecodeShelleyGenesis shelleyPath
      Nothing -> do
        -- No template given: a default file is created
        pure $ shelleyGenesisDefaults { sgNetworkMagic = unNetworkMagic (toNetworkMagic networkId) }

  let -- {0 -> genesis-keys/genesis0/key.vkey, 1 -> genesis-keys/genesis1/key.vkey, ...}
      genesisVKeysPaths = mkPaths numGenesisKeys genesisDir "genesis" "key.vkey"
      -- {0 -> delegate-keys/delegate0/key.vkey, 1 -> delegate-keys/delegate1/key.vkey, ...}
      delegateKeys = mkPaths numGenesisKeys delegateDir "delegate" "key.vkey"
      -- {0 -> delegate-keys/delegate0/vrf.vkey, 1 -> delegate-keys/delegate1/vrf.vkey, ...}
      delegateVrfKeys = mkPaths numGenesisKeys delegateDir "delegate" "vrf.vkey"
      -- {"stake-delegators/delegator1", "stake-delegators/delegator2", ...}
      stakeDelegatorsDirs = [stakeDelegatorsDir </> "delegator" <> show i | i <- [1 .. numStakeDelegators]]

  forM_ [ 1 .. numGenesisKeys ] $ \index -> do
    createGenesisKeys (genesisDir </> ("genesis" <> show index))
    createDelegateKeys desiredKeyOutputFormat (delegateDir </> ("delegate" <> show index))

  when (0 < numGenesisKeys) $ do
    writeREADME genesisDir genesisREADME
    writeREADME delegateDir delegatesREADME

  -- UTxO keys
  let utxoKeys = [utxoKeysDir </> ("utxo" <> show index) </> "utxo.vkey"
                  | index <-  [ 1 .. numUtxoKeys ]]
  forM_ [ 1 .. numUtxoKeys ] $ \index ->
    createUtxoKeys $ utxoKeysDir </> ("utxo" <> show index)

  when (0 < numUtxoKeys) $ writeREADME utxoKeysDir utxoKeysREADME

  let mayStakePoolRelays = Nothing -- TODO @smelc temporary?

  -- Pools
  poolParams <- forM [ 1 .. numPools ] $ \index -> do
    let poolDir = poolsDir </> ("pool" <> show index)

    createPoolCredentials desiredKeyOutputFormat poolDir
    buildPoolParams networkId poolDir Nothing (fromMaybe mempty mayStakePoolRelays)

  when (0 < numPools) $ writeREADME poolsDir poolsREADME

  -- DReps
  forM_ [ 1 .. numDrepKeys ] $ \index -> do
    let drepDir = drepsDir </> "drep" <> show index
        vkeyFile = File @(VerificationKey ()) $ drepDir </> "drep.vkey"
        skeyFile = File @(SigningKey ()) $ drepDir </> "drep.skey"
        cmd = DRep.GovernanceDRepKeyGenCmdArgs ConwayEraOnwardsConway vkeyFile skeyFile
    liftIO $ createDirectoryIfMissing True drepDir
    firstExceptT GenesisCmdFileError $ DRep.runGovernanceDRepKeyGenCmd cmd

  when (0 < numDrepKeys) $ writeREADME drepsDir drepsREADME

  -- Stake delegators
  case stakeDelegators of
    OnDisk _ ->
      forM_ [ 1 .. numStakeDelegators] $ \index -> do
        createStakeDelegatorCredentials (stakeDelegatorsDir </> "delegator" <> show index)
    Transient _ -> pure ()

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
  delegations <-
    case stakeDelegators of
      OnDisk 0 ->
        -- Required because the most general case below loops in this case
        -- (try @zipWith _ (concat $ repeat []) _@ in a REPL)
        pure []
      OnDisk _ -> do
        let delegates = concat $ repeat stakeDelegatorsDirs
        -- We don't need to be attentive to laziness here, because anyway this
        -- doesn't scale really well (because we're generating legit credentials,
        -- as opposed to the Transient case).
        zipWithM (computeDelegation networkId) delegates distribution
      Transient _ ->
        liftIO $ Lazy.forStateM g distribution $ flip computeInsecureDelegation networkId

  genDlgs <- readGenDelegsMap genesisVKeysPaths delegateKeys delegateVrfKeys
  nonDelegAddrs <- readInitialFundAddresses utxoKeys networkId
  start <- maybe (SystemStart <$> getCurrentTimePlus30) pure systemStart

  let network = toShelleyNetwork networkId
  stuffedUtxoAddrs <- liftIO $ Lazy.replicateM (fromIntegral numStuffedUtxo) $ genStuffedAddress network

  let stake = second Ledger.ppId . mkDelegationMapEntry <$> delegations
      stakePools = [ (Ledger.ppId poolParams', poolParams') | poolParams' <- snd . mkDelegationMapEntry <$> delegations ]
      delegAddrs = dInitialUtxoAddr <$> delegations
      !shelleyGenesis' =
        updateOutputTemplate
          start genDlgs totalSupply nonDelegAddrs stakePools stake
          delegatedSupply (length delegations) delegAddrs stuffedUtxoAddrs shelleyGenesis

  -- Write genesis.json file to output
  liftIO $ LBS.writeFile (outputDir </> "genesis.json") $ Aeson.encode shelleyGenesis'
  where
    genesisDir = outputDir </> "genesis-keys"
    delegateDir = outputDir </> "delegate-keys"
    drepsDir = outputDir </> "drep-keys"
    utxoKeysDir = outputDir </> "utxo-keys"
    poolsDir = outputDir </> "pools-keys"
    stakeDelegatorsDir = outputDir </> "stake-delegators"
    numStakeDelegators = case stakeDelegators of OnDisk n -> n; Transient n -> n
    mkDelegationMapEntry :: Delegation -> (Ledger.KeyHash Ledger.Staking StandardCrypto, Ledger.PoolParams StandardCrypto)
    mkDelegationMapEntry d = (dDelegStaking d, dPoolParams d)

-- | The output format used all along this file
desiredKeyOutputFormat :: KeyOutputFormat
desiredKeyOutputFormat = KeyOutputFormatTextEnvelope

writeREADME :: ()
  => FilePath
  -> Text.Text
  -> ExceptT GenesisCmdError IO ()
writeREADME dir content = do
  firstExceptT GenesisCmdFileError . newExceptT $ writeTextFile file content
  where
    file :: File Text.Text Out = File $ dir </> "README.md"

genesisREADME :: Text.Text
genesisREADME = Text.intercalate "\n"
  ["Keys generated by the --genesis-keys flag. In Byron these keys were used to mint blocks and initiate hard forks."
   , "Starting with Shelley and decentralization, blocks started being produced by other keys than genesis keys."
   , "Still, these keys were required to trigger hard forks."
   , "With the introduction of Conway, these keys should become useless"]

delegatesREADME :: Text.Text
delegatesREADME = Text.intercalate "\n"
  ["Keys generated by the --genesis-keys flag. These keys are used to mint blocks when not being completely decentralized",
   "(e.g. when stake pools are not the sole block producers). These keys are intended to run nodes."]

drepsREADME :: Text.Text
drepsREADME = Text.intercalate "\n"
  ["Keys generated by the --drep-keys flag."]

utxoKeysREADME :: Text.Text
utxoKeysREADME = Text.intercalate "\n"
  ["Keys generated by the --utxo-keys flag. These keys receive a portion of the supply."]

poolsREADME :: Text.Text
poolsREADME = Text.intercalate "\n"
  ["Keys generated by the --pools flag. These keys are intended to run nodes."]

-- | @mkPaths numKeys dir segment filename@ returns the paths to the keys to generate.
-- For example @mkPaths 3 dir prefix fn.ext@ returns
-- [dir/segment1/fn.ext, dir/segment2/fn.ext, dir/segment3/fn.ext]
mkPaths :: Word -> String -> String -> String -> Map Int FilePath
mkPaths numKeys dir segment filename =
  fromList [(fromIntegral idx, dir </> (segment <> show idx) </> filename)
            | idx <- [1 .. numKeys]]

genStuffedAddress :: Ledger.Network -> IO (AddressInEra ShelleyEra)
genStuffedAddress network =
  shelleyAddressInEra ShelleyBasedEraShelley <$>
  (ShelleyAddress
   <$> pure network
   <*> (Ledger.KeyHashObj . mkKeyHash . read64BitInt
         <$> Crypto.runSecureRandom (getRandomBytes 8))
   <*> pure Ledger.StakeRefNull)
   where
    read64BitInt :: ByteString -> Int
    read64BitInt = (fromIntegral :: Word64 -> Int)
      . Bin.runGet Bin.getWord64le . LBS.fromStrict

    mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
    mkDummyHash _ = coerce . Ledger.hashWithSerialiser @h toCBOR

    mkKeyHash :: forall c discriminator. Crypto c => Int -> Ledger.KeyHash discriminator c
    mkKeyHash = Ledger.KeyHash . mkDummyHash (Proxy @(ADDRHASH c))

createDelegateKeys :: KeyOutputFormat -> FilePath -> ExceptT GenesisCmdError IO ()
createDelegateKeys fmt dir = do
  liftIO $ createDirectoryIfMissing True dir
  runGenesisKeyGenDelegateCmd
    Cmd.GenesisKeyGenDelegateCmdArgs
    { Cmd.verificationKeyPath = File @(VerificationKey ()) $ dir </> "key.vkey"
    , Cmd.signingKeyPath = onlyOut coldSK
    , Cmd.opCertCounterPath = onlyOut opCertCtr
    }
  runGenesisKeyGenDelegateVRF
        (File @(VerificationKey ()) $ dir </> "vrf.vkey")
        (File @(SigningKey ()) $ dir </> "vrf.skey")
  firstExceptT GenesisCmdNodeCmdError $ do
    runNodeKeyGenKesCmd $ Cmd.NodeKeyGenKESCmdArgs
        fmt
        (onlyOut kesVK)
        (File @(SigningKey ()) $ dir </> "kes.skey")
    runNodeIssueOpCertCmd $ Cmd.NodeIssueOpCertCmdArgs
        (VerificationKeyFilePath (onlyIn kesVK))
        (onlyIn coldSK)
        opCertCtr
        (KESPeriod 0)
        (File $ dir </> "opcert.cert")
 where
   kesVK = File @(VerificationKey ()) $ dir </> "kes.vkey"
   coldSK = File @(SigningKey ()) $ dir </> "key.skey"
   opCertCtr = File $ dir </> "opcert.counter"

createGenesisKeys :: FilePath -> ExceptT GenesisCmdError IO ()
createGenesisKeys dir = do
  liftIO $ createDirectoryIfMissing True dir
  runGenesisKeyGenGenesisCmd
    GenesisKeyGenGenesisCmdArgs
    { verificationKeyPath = File @(VerificationKey ()) $ dir </> "key.vkey"
    , signingKeyPath = File @(SigningKey ()) $ dir </> "key.skey"
    }

createStakeDelegatorCredentials :: FilePath -> ExceptT GenesisCmdError IO ()
createStakeDelegatorCredentials dir = do
  liftIO $ createDirectoryIfMissing True dir
  firstExceptT GenesisCmdAddressCmdError $
    runAddressKeyGenCmd desiredKeyOutputFormat AddressKeyShelley paymentVK paymentSK
  firstExceptT GenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGenCmd desiredKeyOutputFormat stakingVK stakingSK
  where
    paymentVK = File @(VerificationKey ()) $ dir </> "payment.vkey"
    paymentSK = File @(SigningKey ()) $ dir </> "payment.skey"
    stakingVK = File @(VerificationKey ()) $ dir </> "staking.vkey"
    stakingSK = File @(SigningKey ()) $ dir </> "staking.skey"

createUtxoKeys :: FilePath -> ExceptT GenesisCmdError IO ()
createUtxoKeys dir = do
  liftIO $ createDirectoryIfMissing True dir
  runGenesisKeyGenUTxOCmd
    Cmd.GenesisKeyGenUTxOCmdArgs
    { Cmd.verificationKeyPath = File @(VerificationKey ()) $ dir </> "utxo.vkey"
    , Cmd.signingKeyPath = File @(SigningKey ()) $ dir </> "utxo.skey"
    }

createPoolCredentials :: KeyOutputFormat -> FilePath -> ExceptT GenesisCmdError IO ()
createPoolCredentials fmt dir = do
  liftIO $ createDirectoryIfMissing True dir
  firstExceptT GenesisCmdNodeCmdError $ do
    runNodeKeyGenKesCmd $ Cmd.NodeKeyGenKESCmdArgs
        fmt
        (onlyOut kesVK)
        (File @(SigningKey ()) $ dir </> "kes.skey")
    runNodeKeyGenVrfCmd $ Cmd.NodeKeyGenVRFCmdArgs
        fmt
        (File @(VerificationKey ()) $ dir </> "vrf.vkey")
        (File @(SigningKey ()) $ dir </> "vrf.skey")
    runNodeKeyGenColdCmd $ Cmd.NodeKeyGenColdCmdArgs
        fmt
        (File @(VerificationKey ()) $ dir </> "cold.vkey")
        (onlyOut coldSK)
        (onlyOut opCertCtr)
    runNodeIssueOpCertCmd $ Cmd.NodeIssueOpCertCmdArgs
        (VerificationKeyFilePath (onlyIn kesVK))
        (onlyIn coldSK)
        opCertCtr
        (KESPeriod 0)
        (File $ dir </> "opcert.cert")
  firstExceptT GenesisCmdStakeAddressCmdError $
    runStakeAddressKeyGenCmd
        fmt
        (File @(VerificationKey ()) $ dir </> "staking-reward.vkey")
        (File @(SigningKey ()) $ dir </> "staking-reward.skey")
 where
   kesVK = File @(VerificationKey ()) $ dir </> "kes.vkey"
   coldSK = File @(SigningKey ()) $ dir </> "cold.skey"
   opCertCtr = File $ dir </> "opcert.counter"

data Delegation = Delegation
  { dInitialUtxoAddr  :: !(AddressInEra ShelleyEra)
  , dDelegStaking     :: !(Ledger.KeyHash Ledger.Staking StandardCrypto)
  , dPoolParams       :: !(Ledger.PoolParams StandardCrypto)
  }
  deriving (Generic, NFData)

buildPoolParams
  :: NetworkId
  -> FilePath -- ^ File directory where the necessary pool credentials were created
  -> Maybe Word
  -> Map Word [Ledger.StakePoolRelay] -- ^ User submitted stake pool relay map
  -> ExceptT GenesisCmdError IO (Ledger.PoolParams StandardCrypto)
buildPoolParams nw dir index specifiedRelays = do
    StakePoolVerificationKey poolColdVK
      <- firstExceptT (GenesisCmdStakePoolCmdError . StakePoolCmdReadFileError)
           . newExceptT $ readFileTextEnvelope (AsVerificationKey AsStakePoolKey) poolColdVKF

    VrfVerificationKey poolVrfVK
      <- firstExceptT (GenesisCmdNodeCmdError . NodeCmdReadFileError)
           . newExceptT $ readFileTextEnvelope (AsVerificationKey AsVrfKey) poolVrfVKF
    rewardsSVK
      <- firstExceptT GenesisCmdTextEnvReadFileError
           . newExceptT $ readFileTextEnvelope (AsVerificationKey AsStakeKey) poolRewardVKF

    pure Ledger.PoolParams
      { Ledger.ppId          = Ledger.hashKey poolColdVK
      , Ledger.ppVrf         = Ledger.hashVerKeyVRF poolVrfVK
      , Ledger.ppPledge      = Ledger.Coin 0
      , Ledger.ppCost        = Ledger.Coin 0
      , Ledger.ppMargin      = minBound
      , Ledger.ppRewardAcnt  =
          toShelleyStakeAddr $ makeStakeAddress nw $ StakeCredentialByKey (verificationKeyHash rewardsSVK)
      , Ledger.ppOwners      = mempty
      , Ledger.ppRelays      = lookupPoolRelay specifiedRelays
      , Ledger.ppMetadata    = Ledger.SNothing
      }
 where
   lookupPoolRelay
     :: Map Word [Ledger.StakePoolRelay] -> Seq.StrictSeq Ledger.StakePoolRelay
   lookupPoolRelay m =
      case index of
        Nothing -> mempty
        Just index' -> maybe mempty Seq.fromList (Map.lookup index' m)

   strIndex = maybe "" show index
   poolColdVKF = File $ dir </> "cold" ++ strIndex ++ ".vkey"
   poolVrfVKF = File $ dir </> "vrf" ++ strIndex ++ ".vkey"
   poolRewardVKF = File $ dir </> "staking-reward" ++ strIndex ++ ".vkey"

computeDelegation
  :: NetworkId
  -> FilePath
  -> Ledger.PoolParams StandardCrypto
  -> ExceptT GenesisCmdError IO Delegation
computeDelegation nw delegDir dPoolParams = do
    payVK   <- readVKeyFromDisk AsPaymentKey payVKF
    stakeVK <- readVKeyFromDisk AsStakeKey   stakeVKF
    let paymentCredential = PaymentCredentialByKey $ verificationKeyHash payVK
        stakeAddrRef = StakeAddressByValue $ StakeCredentialByKey $ verificationKeyHash stakeVK
        dInitialUtxoAddr = makeShelleyAddressInEra ShelleyBasedEraShelley nw paymentCredential stakeAddrRef
        dDelegStaking = Ledger.hashKey $ unStakeVerificationKey stakeVK

    pure $ Delegation { dInitialUtxoAddr, dDelegStaking, dPoolParams }
   where
     payVKF = File @(VerificationKey ()) $ delegDir </> "payment.vkey"
     stakeVKF = File @(VerificationKey ()) $ delegDir </> "staking.vkey"
     readVKeyFromDisk role file =
      firstExceptT GenesisCmdFileInputDecodeError $ newExceptT $
        Keys.readVerificationKeyOrFile role (VerificationKeyFilePath file)

-- | This function should only be used for testing purposes.
-- Keys returned by this function are not cryptographically secure.
computeInsecureDelegation
  :: StdGen
  -> NetworkId
  -> Ledger.PoolParams StandardCrypto
  -> IO (StdGen, Delegation)
computeInsecureDelegation g0 nw pool = do
    (paymentVK, g1) <- first getVerificationKey <$> generateInsecureSigningKey g0 AsPaymentKey
    (stakeVK  , g2) <- first getVerificationKey <$> generateInsecureSigningKey g1 AsStakeKey

    let stakeAddressReference = StakeAddressByValue . StakeCredentialByKey . verificationKeyHash $ stakeVK
    let initialUtxoAddr = makeShelleyAddress nw (PaymentCredentialByKey (verificationKeyHash paymentVK)) stakeAddressReference

    delegation <- pure $ force Delegation
      { dInitialUtxoAddr = shelleyAddressInEra ShelleyBasedEraShelley initialUtxoAddr
      , dDelegStaking = Ledger.hashKey (unStakeVerificationKey stakeVK)
      , dPoolParams = pool
      }

    pure (g2, delegation)


updateOutputTemplate
    :: SystemStart -- ^ System start time
    -> Map (Hash GenesisKey) (Hash GenesisDelegateKey, Hash VrfKey) -- ^ Genesis delegation (not stake-based)
    -> Maybe Lovelace -- ^ Total amount of lovelace
    -> [AddressInEra ShelleyEra] -- ^ UTxO addresses that are not delegating
    -> [(Ledger.KeyHash 'Ledger.StakePool StandardCrypto, Ledger.PoolParams StandardCrypto)] -- ^ Pool map
    -> [(Ledger.KeyHash 'Ledger.Staking StandardCrypto, Ledger.KeyHash 'Ledger.StakePool StandardCrypto)] -- ^ Delegaton map
    -> Maybe Lovelace -- ^ Amount of lovelace to delegate
    -> Int -- ^ Number of UTxO address for delegation
    -> [AddressInEra ShelleyEra] -- ^ UTxO address for delegation
    -> [AddressInEra ShelleyEra] -- ^ Stuffed UTxO addresses
    -> ShelleyGenesis StandardCrypto -- ^ Template from which to build a genesis
    -> ShelleyGenesis StandardCrypto -- ^ Updated genesis
updateOutputTemplate
  (SystemStart sgSystemStart)
  genDelegMap mTotalSupply utxoAddrsNonDeleg pools stake
  mDelegatedSupply
  nUtxoAddrsDeleg utxoAddrsDeleg stuffedUtxoAddrs
  template@ShelleyGenesis{ sgProtocolParams } =
    template
          { sgSystemStart
          , sgMaxLovelaceSupply = totalSupply
          , sgGenDelegs = shelleyDelKeys
          , sgInitialFunds = ListMap.fromList
                              [ (toShelleyAddr addr, toShelleyLovelace v)
                              | (addr, v) <-
                                distribute (nonDelegCoin - subtractForTreasury) nUtxoAddrsNonDeleg  utxoAddrsNonDeleg
                                ++
                                distribute (delegCoin - subtractForTreasury)    nUtxoAddrsDeleg     utxoAddrsDeleg
                                ++
                                mkStuffedUtxo stuffedUtxoAddrs
                                ]
          , sgStaking =
            ShelleyGenesisStaking
              { sgsPools = ListMap pools
              , sgsStake = ListMap stake
              }
          , sgProtocolParams
          }
  where
    nUtxoAddrsNonDeleg  = length utxoAddrsNonDeleg
    maximumLovelaceSupply :: Word64
    maximumLovelaceSupply = sgMaxLovelaceSupply template
    -- If the initial funds are equal to the maximum funds, rewards cannot be created.
    subtractForTreasury :: Integer
    subtractForTreasury = nonDelegCoin `quot` 10
    totalSupply :: Word64
    -- if --total-supply is not specified, supply comes from the template passed to this function:
    totalSupply = maybe maximumLovelaceSupply unLovelace mTotalSupply
    delegCoin, nonDelegCoin :: Integer
    delegCoin = case mDelegatedSupply of Nothing -> 0; Just amountDeleg -> fromIntegral totalSupply - unLovelace amountDeleg
    nonDelegCoin = fromIntegral totalSupply - delegCoin

    distribute :: Integer -> Int -> [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    distribute funds nAddrs addrs = zip addrs (fmap Lovelace (coinPerAddr + remainder:repeat coinPerAddr))
      where coinPerAddr, remainder :: Integer
            (coinPerAddr, remainder) = funds `divMod` fromIntegral nAddrs

    mkStuffedUtxo :: [AddressInEra ShelleyEra] -> [(AddressInEra ShelleyEra, Lovelace)]
    mkStuffedUtxo xs = (, Lovelace minUtxoVal) <$> xs
      where Coin minUtxoVal = sgProtocolParams ^. ppMinUTxOValueL

    shelleyDelKeys = Map.fromList
      [ (gh, Ledger.GenDelegPair gdh h)
      | (GenesisKeyHash gh,
          (GenesisDelegateKeyHash gdh, VrfKeyHash h)) <- Map.toList genDelegMap
      ]

    unLovelace :: Integral a => Lovelace -> a
    unLovelace (Lovelace coin) = fromIntegral coin

readAndDecodeShelleyGenesis
  :: FilePath
  -> IO (Either GenesisCmdError (ShelleyGenesis StandardCrypto))
readAndDecodeShelleyGenesis fpath = runExceptT $ do
  lbs <- handleIOExceptT (GenesisCmdGenesisFileReadError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (GenesisCmdGenesisFileDecodeError fpath . Text.pack)
    . hoistEither $ Aeson.eitherDecode' lbs

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: ExceptT a IO UTCTime
getCurrentTimePlus30 =
    plus30sec <$> liftIO getCurrentTime
  where
    plus30sec :: UTCTime -> UTCTime
    plus30sec = addUTCTime (30 :: NominalDiffTime)

readGenDelegsMap :: Map Int FilePath
                 -> Map Int FilePath
                 -> Map Int FilePath
                 -> ExceptT GenesisCmdError IO
                            (Map (Hash GenesisKey)
                                 (Hash GenesisDelegateKey, Hash VrfKey))
readGenDelegsMap genesisKeys delegateKeys delegateVrfKeys = do
    gkm <- readKeys (AsVerificationKey AsGenesisKey) genesisKeys
    dkm <- readKeys (AsVerificationKey AsGenesisDelegateKey) delegateKeys
    vkm <- readKeys (AsVerificationKey AsVrfKey) delegateVrfKeys

    let combinedMap :: Map Int (VerificationKey GenesisKey,
                                (VerificationKey GenesisDelegateKey,
                                 VerificationKey VrfKey))
        combinedMap =
          Map.intersectionWith (,)
            gkm
            (Map.intersectionWith (,) dkm vkm)

    -- All the maps should have an identical set of keys. Complain if not.
    let gkmExtra = gkm Map.\\ combinedMap
        dkmExtra = dkm Map.\\ combinedMap
        vkmExtra = vkm Map.\\ combinedMap
    unless (Map.null gkmExtra && Map.null dkmExtra && Map.null vkmExtra) $
      throwError $ GenesisCmdMismatchedGenesisKeyFiles
                     (Map.keys gkm) (Map.keys dkm) (Map.keys vkm)

    let delegsMap :: Map (Hash GenesisKey)
                         (Hash GenesisDelegateKey, Hash VrfKey)
        delegsMap =
          Map.fromList [ (gh, (dh, vh))
                       | (g,(d,v)) <- Map.elems combinedMap
                       , let gh = verificationKeyHash g
                             dh = verificationKeyHash d
                             vh = verificationKeyHash v
                       ]

    pure delegsMap


-- | Given a map @{0 -> someKey0, 1 -> someKey1}@, lift reading
-- the files to the map's values.
readKeys :: ()
  => HasTextEnvelope a
  => Ord k
  => AsType a
  -> Map k FilePath
  -> ExceptT GenesisCmdError IO (Map k a)
readKeys asType genesisVKeys = do
  firstExceptT GenesisCmdTextEnvReadFileError $
    Map.fromList <$>
      sequence
        [ (,) ix <$> readKey (File file)
        | (ix, file) <- toList genesisVKeys ]
  where
    readKey = newExceptT . readFileTextEnvelope asType


readInitialFundAddresses :: [FilePath] -> NetworkId
                         -> ExceptT GenesisCmdError IO [AddressInEra ShelleyEra]
readInitialFundAddresses utxoKeys nw = do
    vkeys <- firstExceptT GenesisCmdTextEnvReadFileError $
               sequence
                 [ newExceptT $
                     readFileTextEnvelope (AsVerificationKey AsGenesisUTxOKey)
                                          (File file)
                 | file <- utxoKeys ]
    return [ addr | vkey <- vkeys
           , let vkh  = verificationKeyHash (castVerificationKey vkey)
                 addr = makeShelleyAddressInEra ShelleyBasedEraShelley nw (PaymentCredentialByKey vkh)
                                                NoStakeAddress
           ]
