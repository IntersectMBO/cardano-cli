{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

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

import Cardano.Api.Byron (ByronKey, NetworkId, SigningKey (..), toByronRequiresNetworkMagic)
import Cardano.Api.Byron qualified as Byron
import Cardano.Api.Error
import Cardano.Api.IO
import Cardano.Api.Key
import Cardano.Api.Monad.Error
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Raw

import Cardano.CLI.Byron.Delegation
import Cardano.CLI.Byron.Key
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common (GenesisFile (..))
import Cardano.Crypto qualified as Crypto
import Cardano.Prelude (canonicalDecodePretty, canonicalEncodePretty)

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LB
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.String (IsString)
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time (UTCTime)
import Formatting.Buildable
import System.Directory (createDirectory, doesPathExist)

data ByronGenesisError
  = GenesisOutputDirAlreadyExists FilePath
  | GenesisReadError !FilePath !Byron.GenesisDataError
  | GenesisSpecError !Text
  | MakeGenesisDelegationError !Byron.GenesisDelegationError
  | NoGenesisDelegationForKey !Text
  | ProtocolParametersParseFailed !FilePath !Text
  deriving Show

instance Error ByronGenesisError where
  prettyError = renderByronGenesisError

renderByronGenesisError :: ByronGenesisError -> Doc ann
renderByronGenesisError = \case
  ProtocolParametersParseFailed pParamFp parseError ->
    "Protocol parameters parse failed at: " <> pshow pParamFp <> " Error: " <> pretty parseError
  MakeGenesisDelegationError genDelegError ->
    "Error creating genesis delegation: " <> pshow genDelegError
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
  , gpK :: !Byron.BlockCount
  , gpProtocolMagic :: !Crypto.ProtocolMagic
  , gpTestnetBalance :: !Byron.TestnetBalanceOptions
  , gpFakeAvvmOptions :: !Byron.FakeAvvmOptions
  , gpAvvmBalanceFactor :: !Byron.LovelacePortion
  , gpSeed :: !(Maybe Integer)
  }
  deriving Show

mkGenesisSpec :: GenesisParameters -> ExceptT ByronGenesisError IO Byron.GenesisSpec
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
      Byron.mkGenesisDelegation []

  withExceptT GenesisSpecError $
    ExceptT . pure $
      Byron.mkGenesisSpec
        (Byron.GenesisAvvmBalances mempty)
        genesisDelegation
        protocolParameters
        (gpK gp)
        (gpProtocolMagic gp)
        (mkGenesisInitialiser True)
 where
  mkGenesisInitialiser :: Bool -> Byron.GenesisInitializer
  mkGenesisInitialiser =
    Byron.GenesisInitializer
      (gpTestnetBalance gp)
      (gpFakeAvvmOptions gp)
      (Byron.lovelacePortionToRational (gpAvvmBalanceFactor gp))

-- | Generate a genesis, for given blockchain start time, protocol parameters,
-- security parameter, protocol magic, testnet balance options, fake AVVM options,
-- AVVM balance factor and seed.  Throw an error in the following cases: if the
-- protocol parameters file can't be read or fails parse, if genesis delegation
-- couldn't be generated, if the parameter-derived genesis specification is wrong,
-- or if the genesis fails generation.
mkGenesis
  :: GenesisParameters
  -> CIO e (Byron.GenesisData, Byron.GeneratedSecrets)
mkGenesis gp = do
  genesisSpec <- fromExceptTCli $ mkGenesisSpec gp

  fromExceptTCli $
    Byron.generateGenesisData (gpStartTime gp) genesisSpec

-- | Read genesis from a file.
readGenesis
  :: GenesisFile
  -> NetworkId
  -> ExceptT ByronGenesisError IO Byron.Config
readGenesis (GenesisFile file) nw =
  firstExceptT (GenesisReadError file) $ do
    (genesisData, genesisHash) <- Byron.readGenesisData file
    return
      Byron.Config
        { Byron.configGenesisData = genesisData
        , Byron.configGenesisHash = genesisHash
        , Byron.configReqNetMagic = toByronRequiresNetworkMagic nw
        , Byron.configUTxOConfiguration = Byron.defaultUTxOConfiguration
        }

-- | Write out genesis into a directory that must not yet exist.  An error is
-- thrown if the directory already exists, or the genesis has delegate keys that
-- are not delegated to.
dumpGenesis
  :: NewDirectory
  -> Byron.GenesisData
  -> Byron.GeneratedSecrets
  -> ExceptT ByronGenesisError IO ()
dumpGenesis (NewDirectory outDir) genesisData gs = do
  exists <- liftIO $ doesPathExist outDir
  if exists
    then left $ GenesisOutputDirAlreadyExists outDir
    else liftIO $ createDirectory outDir
  liftIO $ LB.writeFile genesisJSONFile (canonicalEncodePretty genesisData)

  dlgCerts <- mapM (findDelegateCert . ByronSigningKey) $ Byron.gsRichSecrets gs

  liftIO $
    wOut
      "genesis-keys"
      "key"
      serialiseToRawBytes
      (map ByronSigningKey $ Byron.gsDlgIssuersSecrets gs)
  liftIO $
    wOut
      "delegate-keys"
      "key"
      serialiseToRawBytes
      (map ByronSigningKey $ Byron.gsRichSecrets gs)
  liftIO $
    wOut
      "poor-keys"
      "key"
      serialiseToRawBytes
      (map (ByronSigningKey . Byron.poorSecretToKey) $ Byron.gsPoorSecrets gs)
  liftIO $ wOut "delegation-cert" "json" serialiseDelegationCert dlgCerts
  liftIO $ wOut "avvm-secrets" "secret" printFakeAvvmSecrets $ Byron.gsFakeAvvmSecrets gs
 where
  dlgCertMap = Byron.unGenesisDelegation $ Byron.gdHeavyDelegation genesisData

  findDelegateCert :: SigningKey ByronKey -> ExceptT ByronGenesisError IO Byron.Certificate
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
  isCertForSK :: Crypto.SigningKey -> Byron.Certificate -> Bool
  isCertForSK sk cert = Byron.delegateVK cert == Crypto.toVerification sk

  wOut :: String -> String -> (a -> ByteString) -> [a] -> IO ()
  wOut = writeSecrets outDir
