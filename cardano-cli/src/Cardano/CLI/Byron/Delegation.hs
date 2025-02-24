{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Delegation
  ( ByronDelegationError (..)
  , checkByronGenesisDelegation
  , issueByronGenesisDelegation
  , renderByronDelegationError
  , serialiseDelegationCert
  , serialiseByronWitness
  )
where

import Cardano.Api.Byron (ACertificate (delegateVK))
import Cardano.Api.Byron hiding (delegateVK)
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Byron.Key (ByronKeyFailure, renderByronKeyFailure)
import Cardano.CLI.Type.Common (CertificateFile (..))
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Crypto qualified as Crypto
import Cardano.Prelude (canonicalDecodePretty, canonicalEncodePretty)

import Prelude hiding ((.))

import Control.Category
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LB
import Data.Text (Text)
import Formatting (Format, sformat)

data ByronDelegationError
  = CertificateValidationErrors !FilePath ![Text]
  | DlgCertificateDeserialisationFailed !FilePath !Text
  | ByronDelegationKeyError !ByronKeyFailure
  deriving Show

renderByronDelegationError :: ByronDelegationError -> Doc ann
renderByronDelegationError = \case
  CertificateValidationErrors certFp errs ->
    "Certificate validation error(s) at: " <> pshow certFp <> " Errors: " <> pshow errs
  DlgCertificateDeserialisationFailed certFp deSererr ->
    "Certificate deserialisation error at: " <> pshow certFp <> " Error: " <> pshow deSererr
  ByronDelegationKeyError kerr ->
    renderByronKeyFailure kerr

-- TODO:  we need to support password-protected secrets.

-- | Issue a certificate for genesis delegation to a delegate key, signed by the
--   issuer key, for a given protocol magic and coming into effect at given epoch.
issueByronGenesisDelegation
  :: ProtocolMagicId
  -> EpochNumber
  -> Crypto.SigningKey
  -> Crypto.VerificationKey
  -> Certificate
issueByronGenesisDelegation magic epoch issuerSK delegateVK' =
  signCertificate magic delegateVK' epoch $
    Crypto.noPassSafeSigner issuerSK

-- | Verify that a certificate signifies genesis delegation by assumed genesis key
--   to a delegate key, for a given protocol magic.
--   If certificate fails validation, throw an error.
checkByronGenesisDelegation
  :: CertificateFile
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey
  -> ExceptT ByronDelegationError IO ()
checkByronGenesisDelegation (CertificateFile certF) magic issuer delegate = do
  ecert <- liftIO $ canonicalDecodePretty <$> LB.readFile certF
  case ecert of
    Left e -> left $ DlgCertificateDeserialisationFailed certF e
    Right (cert :: Certificate) -> do
      let issues = checkDlgCert cert magic issuer delegate
      unless (null issues) $
        left $
          CertificateValidationErrors certF issues

checkDlgCert
  :: ACertificate a
  -> ProtocolMagicId
  -> Crypto.VerificationKey
  -> Crypto.VerificationKey
  -> [Text]
checkDlgCert cert magic issuerVK' delegateVK' =
  mconcat
    [ [ sformat "Certificate does not have a valid signature."
      | not (isValid magic' cert')
      ]
    , [ sformat
          ("Certificate issuer " . vkF . " doesn't match expected: " . vkF)
          (issuerVK cert)
          issuerVK'
      | issuerVK cert /= issuerVK'
      ]
    , [ sformat
          ("Certificate delegate " . vkF . " doesn't match expected: " . vkF)
          (delegateVK cert)
          delegateVK'
      | delegateVK cert /= delegateVK'
      ]
    ]
 where
  magic' :: L.Annotated ProtocolMagicId ByteString
  magic' = L.Annotated magic (L.serialize' L.byronProtVer magic)

  epoch :: EpochNumber
  epoch = L.unAnnotated $ aEpoch cert

  cert' :: ACertificate ByteString
  cert' =
    let unannotated =
          cert
            { aEpoch = L.Annotated epoch ()
            , annotation = ()
            }
     in unannotated
          { annotation = L.serialize' L.byronProtVer unannotated
          , aEpoch = L.Annotated epoch (L.serialize' L.byronProtVer epoch)
          }

  vkF :: forall r. Format r (Crypto.VerificationKey -> r)
  vkF = Crypto.fullVerificationKeyF

serialiseDelegationCert :: Certificate -> ByteString
serialiseDelegationCert = LB.toStrict . canonicalEncodePretty

serialiseByronWitness :: SomeByronSigningKey -> ByteString
serialiseByronWitness sk =
  case sk of
    AByronSigningKeyLegacy bSkey -> serialiseToRawBytes bSkey
    AByronSigningKey legBKey -> serialiseToRawBytes legBKey
