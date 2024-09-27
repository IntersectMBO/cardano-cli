{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Byron.Genesis
  ( ByronGenesisError (..)
  , GenesisParameters (..)
  , NewDirectory (..)
  , dumpGenesis
  , mkGenesis
  , readGenesis
  , renderByronGenesisError
  )
where

import           Cardano.Api (Key (..), NetworkId, writeSecrets)
import           Cardano.Api.Byron (ByronKey, SerialiseAsRawBytes (..), SigningKey (..), delegateVK,
                   toByronRequiresNetworkMagic)
import qualified Cardano.Api.Ledger as L

import           Cardano.CLI.Byron.Delegation
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Pretty
import           Cardano.CLI.Types.Common (GenesisFile (..))
import qualified Cardano.Crypto as Crypto
import           Cardano.Prelude (canonicalDecodePretty, canonicalEncodePretty)

import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, right)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Time (UTCTime)
import           Formatting.Buildable
import           System.Directory (createDirectory, doesPathExist)

data ByronGenesisError
  = ByronDelegationCertSerializationError !ByronDelegationError
  | ByronDelegationKeySerializationError ByronDelegationError
  | GenesisGenerationError !L.GenesisDataGenerationError
  | GenesisOutputDirAlreadyExists FilePath
  | GenesisReadError !FilePath !L.GenesisDataError
  | GenesisSpecError !Text
  | MakeGenesisDelegationError !L.GenesisDelegationError
  | NoGenesisDelegationForKey !Text
  | ProtocolParametersParseFailed !FilePath !Text
  | PoorKeyFailure !ByronKeyFailure
  deriving Show

renderByronGenesisError :: ByronGenesisError -> Doc ann
renderByronGenesisError = \case
  ProtocolParametersParseFailed pParamFp parseError ->
    "Protocol parameters parse failed at: " <> pshow pParamFp <> " Error: " <> pretty parseError
  ByronDelegationCertSerializationError bDelegSerErr ->
    "Error while serializing the delegation certificate: " <> pshow bDelegSerErr
  ByronDelegationKeySerializationError bKeySerErr ->
    "Error while serializing the delegation key: " <> pshow bKeySerErr
  PoorKeyFailure bKeyFailure ->
    "Error creating poor keys: " <> pshow bKeyFailure
  MakeGenesisDelegationError genDelegError ->
    "Error creating genesis delegation: " <> pshow genDelegError
  GenesisGenerationError genDataGenError ->
    "Error generating genesis: " <> pshow genDataGenError
  GenesisOutputDirAlreadyExists genOutDir ->
    "Genesis output directory already exists: " <> pshow genOutDir
  GenesisReadError genFp genDataError ->
    "Error while reading genesis file at: " <> pshow genFp <> " Error: " <> pshow genDataError
  GenesisSpecError genSpecError ->
    "Error while creating genesis spec" <> pshow genSpecError
  NoGenesisDelegationForKey verKey ->
    "Error while creating genesis, no delegation certificate for this verification key:" <> pshow verKey

newtype NewDirectory
  = NewDirectory FilePath
  deriving (Eq, Ord, Show, IsString)

-- | Parameters required for generation of new genesis.
data GenesisParameters = GenesisParameters
  { gpStartTime :: !UTCTime
  , gpProtocolParamsFile :: !FilePath
  , gpK :: !L.BlockCount
  , gpProtocolMagic :: !Crypto.ProtocolMagic
  , gpTestnetBalance :: !L.TestnetBalanceOptions
  , gpFakeAvvmOptions :: !L.FakeAvvmOptions
  , gpAvvmBalanceFactor :: !L.LovelacePortion
  , gpSeed :: !(Maybe Integer)
  }
  deriving Show

mkGenesisSpec :: GenesisParameters -> ExceptT ByronGenesisError IO L.GenesisSpec
mkGenesisSpec gp = do
  protoParamsRaw <- lift . LB.readFile $ gpProtocolParamsFile gp

  protocolParameters <-
    withExceptT
      (ProtocolParametersParseFailed (gpProtocolParamsFile gp))
      $ ExceptT . pure
      $ canonicalDecodePretty protoParamsRaw

  -- We're relying on the generator to fake AVVM and delegation.
  genesisDelegation <-
    withExceptT MakeGenesisDelegationError $
      L.mkGenesisDelegation []

  withExceptT GenesisSpecError $
    ExceptT . pure $
      L.mkGenesisSpec
        (L.GenesisAvvmBalances mempty)
        genesisDelegation
        protocolParameters
        (gpK gp)
        (gpProtocolMagic gp)
        (mkGenesisInitialiser True)
 where
  mkGenesisInitialiser :: Bool -> L.GenesisInitializer
  mkGenesisInitialiser =
    L.GenesisInitializer
      (gpTestnetBalance gp)
      (gpFakeAvvmOptions gp)
      (L.lovelacePortionToRational (gpAvvmBalanceFactor gp))

-- | Generate a genesis, for given blockchain start time, protocol parameters,
-- security parameter, protocol magic, testnet balance options, fake AVVM options,
-- AVVM balance factor and seed.  Throw an error in the following cases: if the
-- protocol parameters file can't be read or fails parse, if genesis delegation
-- couldn't be generated, if the parameter-derived genesis specification is wrong,
-- or if the genesis fails generation.
mkGenesis
  :: GenesisParameters
  -> ExceptT ByronGenesisError IO (L.GenesisData, L.GeneratedSecrets)
mkGenesis gp = do
  genesisSpec <- mkGenesisSpec gp

  withExceptT GenesisGenerationError $
    L.generateGenesisData (gpStartTime gp) genesisSpec

-- | Read genesis from a file.
readGenesis
  :: GenesisFile
  -> NetworkId
  -> ExceptT ByronGenesisError IO L.Config
readGenesis (GenesisFile file) nw =
  firstExceptT (GenesisReadError file) $ do
    (genesisData, genesisHash) <- L.readGenesisData file
    return
      L.Config
        { L.configGenesisData = genesisData
        , L.configGenesisHash = genesisHash
        , L.configReqNetMagic = toByronRequiresNetworkMagic nw
        , L.configUTxOConfiguration = L.defaultUTxOConfiguration
        }

-- | Write out genesis into a directory that must not yet exist.  An error is
-- thrown if the directory already exists, or the genesis has delegate keys that
-- are not delegated to.
dumpGenesis
  :: NewDirectory
  -> L.GenesisData
  -> L.GeneratedSecrets
  -> ExceptT ByronGenesisError IO ()
dumpGenesis (NewDirectory outDir) genesisData gs = do
  exists <- liftIO $ doesPathExist outDir
  if exists
    then left $ GenesisOutputDirAlreadyExists outDir
    else liftIO $ createDirectory outDir
  liftIO $ LB.writeFile genesisJSONFile (canonicalEncodePretty genesisData)

  dlgCerts <- mapM (findDelegateCert . ByronSigningKey) $ L.gsRichSecrets gs

  liftIO $
    wOut
      "genesis-keys"
      "key"
      serialiseToRawBytes
      (map ByronSigningKey $ L.gsDlgIssuersSecrets gs)
  liftIO $
    wOut
      "delegate-keys"
      "key"
      serialiseToRawBytes
      (map ByronSigningKey $ L.gsRichSecrets gs)
  liftIO $
    wOut
      "poor-keys"
      "key"
      serialiseToRawBytes
      (map (ByronSigningKey . L.poorSecretToKey) $ L.gsPoorSecrets gs)
  liftIO $ wOut "delegation-cert" "json" serialiseDelegationCert dlgCerts
  liftIO $ wOut "avvm-secrets" "secret" printFakeAvvmSecrets $ L.gsFakeAvvmSecrets gs
 where
  dlgCertMap = L.unGenesisDelegation $ L.gdHeavyDelegation genesisData

  findDelegateCert :: SigningKey ByronKey -> ExceptT ByronGenesisError IO L.Certificate
  findDelegateCert bSkey@(ByronSigningKey sk) =
    case List.find (isCertForSK sk) (Map.elems dlgCertMap) of
      Nothing ->
        left
          . NoGenesisDelegationForKey
          . prettyPublicKey
          $ getVerificationKey bSkey
      Just x -> right x

  genesisJSONFile :: FilePath
  genesisJSONFile = outDir <> "/genesis.json"

  printFakeAvvmSecrets :: Crypto.RedeemSigningKey -> ByteString
  printFakeAvvmSecrets rskey = Text.encodeUtf8 . toStrict . toLazyText $ build rskey

  -- Compare a given 'SigningKey' with a 'Certificate' 'VerificationKey'
  isCertForSK :: Crypto.SigningKey -> L.Certificate -> Bool
  isCertForSK sk cert = delegateVK cert == Crypto.toVerification sk

  wOut :: String -> String -> (a -> ByteString) -> [a] -> IO ()
  wOut = writeSecrets outDir
