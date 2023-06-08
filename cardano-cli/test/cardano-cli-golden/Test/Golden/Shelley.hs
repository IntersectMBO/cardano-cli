{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Shelley
  ( keyConversionTests
  , keyTests
  , certificateTests
  , governancePollTests
  , metadataTests
  , multiSigTests
  , txTests
  ) where

import qualified Hedgehog as H
import           Test.Golden.Shelley.Address.Build (golden_shelleyAddressBuild)
import           Test.Golden.Shelley.Address.Info (golden_shelleyAddressInfo)
import           Test.Golden.Shelley.Address.KeyGen (golden_shelleyAddressExtendedKeyGen,
                   golden_shelleyAddressKeyGen)
import           Test.Golden.Shelley.Genesis.Create (golden_shelleyGenesisCreate)
import           Test.Golden.Shelley.Genesis.InitialTxIn (golden_shelleyGenesisInitialTxIn)
import           Test.Golden.Shelley.Genesis.KeyGenDelegate (golden_shelleyGenesisKeyGenDelegate)
import           Test.Golden.Shelley.Genesis.KeyGenGenesis (golden_shelleyGenesisKeyGenGenesis)
import           Test.Golden.Shelley.Genesis.KeyGenUtxo (golden_shelleyGenesisKeyGenUtxo)
import           Test.Golden.Shelley.Genesis.KeyHash (golden_shelleyGenesisKeyHash)
import           Test.Golden.Shelley.Governance.AnswerPoll (golden_shelleyGovernanceAnswerPoll0,
                   golden_shelleyGovernanceAnswerPollNeg1Invalid,
                   golden_shelleyGovernanceAnswerPollPos1,
                   golden_shelleyGovernanceAnswerPollPos2Invalid)
import           Test.Golden.Shelley.Governance.CreatePoll (golden_shelleyGovernanceCreateLongPoll,
                   golden_shelleyGovernanceCreatePoll)
import           Test.Golden.Shelley.Governance.VerifyPoll (golden_shelleyGovernanceVerifyPoll,
                   golden_shelleyGovernanceVerifyPollInvalidAnswer,
                   golden_shelleyGovernanceVerifyPollMalformedAnswer,
                   golden_shelleyGovernanceVerifyPollMismatch,
                   golden_shelleyGovernanceVerifyPollNoAnswer)
import           Test.Golden.Shelley.Key.ConvertCardanoAddressKey
                   (golden_convertCardanoAddressByronSigningKey,
                   golden_convertCardanoAddressIcarusSigningKey,
                   golden_convertCardanoAddressShelleyPaymentSigningKey,
                   golden_convertCardanoAddressShelleyStakeSigningKey)
import           Test.Golden.Shelley.Metadata.StakePoolMetadata (golden_stakePoolMetadataHash)
import           Test.Golden.Shelley.MultiSig.Address (golden_shelleyAllMultiSigAddressBuild,
                   golden_shelleyAnyMultiSigAddressBuild, golden_shelleyAtLeastMultiSigAddressBuild)
import           Test.Golden.Shelley.Node.IssueOpCert (golden_shelleyNodeIssueOpCert)
import           Test.Golden.Shelley.Node.KeyGen (golden_shelleyNodeKeyGen,
                   golden_shelleyNodeKeyGen_bech32, golden_shelleyNodeKeyGen_te)
import           Test.Golden.Shelley.Node.KeyGenKes (golden_shelleyNodeKeyGenKes,
                   golden_shelleyNodeKeyGenKes_bech32, golden_shelleyNodeKeyGenKes_te)
import           Test.Golden.Shelley.Node.KeyGenVrf (golden_shelleyNodeKeyGenVrf,
                   golden_shelleyNodeKeyGenVrf_bech32, golden_shelleyNodeKeyGenVrf_te)
import           Test.Golden.Shelley.StakeAddress.Build (golden_shelleyStakeAddressBuild)
import           Test.Golden.Shelley.StakeAddress.DeregistrationCertificate
                   (golden_shelleyStakeAddressDeregistrationCertificate)
import           Test.Golden.Shelley.StakeAddress.KeyGen (golden_shelleyStakeAddressKeyGen)
import           Test.Golden.Shelley.StakeAddress.RegistrationCertificate
                   (golden_shelleyStakeAddressRegistrationCertificate)
import           Test.Golden.Shelley.StakePool.RegistrationCertificate
                   (golden_shelleyStakePoolRegistrationCertificate)
import           Test.Golden.Shelley.TextEnvelope.Certificates.GenesisKeyDelegation
                   (golden_shelleyGenesisKeyDelegationCertificate)
import           Test.Golden.Shelley.TextEnvelope.Certificates.MIR (golden_shelleyMIRCertificate)
import           Test.Golden.Shelley.TextEnvelope.Certificates.Operational
                   (golden_shelleyOperationalCertificate)
import           Test.Golden.Shelley.TextEnvelope.Certificates.StakeAddress
                   (golden_shelleyStakeAddressCertificates)
import           Test.Golden.Shelley.TextEnvelope.Certificates.StakePool
                   (golden_shelleyStakePoolCertificates)
import           Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys
                   (golden_shelleyExtendedPaymentKeys, golden_shelleyExtendedPaymentKeys_bech32,
                   golden_shelleyExtendedPaymentKeys_te)
import           Test.Golden.Shelley.TextEnvelope.Keys.GenesisDelegateKeys
                   (golden_shelleyGenesisDelegateKeys)
import           Test.Golden.Shelley.TextEnvelope.Keys.GenesisKeys (golden_shelleyGenesisKeys)
import           Test.Golden.Shelley.TextEnvelope.Keys.GenesisUTxOKeys
                   (golden_shelleyGenesisUTxOKeys)
import           Test.Golden.Shelley.TextEnvelope.Keys.KESKeys (golden_shelleyKESKeys)
import           Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys (golden_shelleyPaymentKeys,
                   golden_shelleyPaymentKeys_bech32, golden_shelleyPaymentKeys_te)
import           Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys (golden_shelleyStakeKeys,
                   golden_shelleyStakeKeys_bech32, golden_shelleyStakeKeys_te)
import           Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys (golden_shelleyVRFKeys,
                   golden_shelleyVRFKeys_bech32, golden_shelleyVRFKeys_te)
import           Test.Golden.Shelley.TextEnvelope.Tx.Tx (golden_shelleyTx)
import           Test.Golden.Shelley.TextEnvelope.Tx.TxBody (golden_shelleyTxBody)
import           Test.Golden.Shelley.TextView.DecodeCbor (golden_shelleyTextViewDecodeCbor)
import           Test.Golden.Shelley.Transaction.Assemble
                   (golden_shelleyTransactionAssembleWitness_SigningKey)
import           Test.Golden.Shelley.Transaction.Build (golden_shelleyTransactionBuild,
                   golden_shelleyTransactionBuild_CertificateScriptWitnessed,
                   golden_shelleyTransactionBuild_Minting,
                   golden_shelleyTransactionBuild_TxInScriptWitnessed,
                   golden_shelleyTransactionBuild_WithdrawalScriptWitnessed)
import           Test.Golden.Shelley.Transaction.CalculateMinFee
                   (golden_shelleyTransactionCalculateMinFee)
import           Test.Golden.Shelley.Transaction.CreateWitness
                   (golden_shelleyTransactionSigningKeyWitness)
import           Test.Golden.Shelley.Transaction.Sign (golden_shelleyTransactionSign)
import           Test.Golden.Version (golden_version)

keyTests :: IO Bool
keyTests =
  H.checkSequential
    $ H.Group "TextEnvelope Key Goldens"
        [ ("golden_shelleyAddressInfo", golden_shelleyAddressInfo)
        , ("golden_shelleyAddressKeyGen", golden_shelleyAddressKeyGen)
        , ("golden_shelleyAddressExtendedKeyGen", golden_shelleyAddressExtendedKeyGen)
        , ("golden_shelleyAddressBuild", golden_shelleyAddressBuild)
        , ("golden_shelleyExtendedPaymentKeys", golden_shelleyExtendedPaymentKeys)
        , ("golden_shelleyExtendedPaymentKeys_bech32", golden_shelleyExtendedPaymentKeys_bech32)
        , ("golden_shelleyExtendedPaymentKeys_te", golden_shelleyExtendedPaymentKeys_te)
        , ("golden_shelleyGenesisCreate", golden_shelleyGenesisCreate)
        , ("golden_shelleyGenesisDelegateKeys", golden_shelleyGenesisDelegateKeys)
        , ("golden_shelleyGenesisInitialTxIn", golden_shelleyGenesisInitialTxIn)
        , ("golden_shelleyGenesisKeyGenDelegate", golden_shelleyGenesisKeyGenDelegate)
        , ("golden_shelleyGenesisKeyGenGenesis", golden_shelleyGenesisKeyGenGenesis)
        , ("golden_shelleyGenesisKeyGenUtxo", golden_shelleyGenesisKeyGenUtxo)
        , ("golden_shelleyGenesisKeyHash", golden_shelleyGenesisKeyHash)
        , ("golden_shelleyGenesisKeys", golden_shelleyGenesisKeys)
        , ("golden_shelleyGenesisUTxOKeys", golden_shelleyGenesisUTxOKeys)
        , ("golden_shelleyKESKeys", golden_shelleyKESKeys)
        , ("golden_shelleyNodeIssueOpCert", golden_shelleyNodeIssueOpCert)
        , ("golden_shelleyNodeKeyGen", golden_shelleyNodeKeyGen)
        , ("golden_shelleyNodeKeyGen_bech32", golden_shelleyNodeKeyGen_bech32)
        , ("golden_shelleyNodeKeyGen_te", golden_shelleyNodeKeyGen_te)
        , ("golden_shelleyNodeKeyGenKes", golden_shelleyNodeKeyGenKes)
        , ("golden_shelleyNodeKeyGenKes_bech32", golden_shelleyNodeKeyGenKes_bech32)
        , ("golden_shelleyNodeKeyGenKes_te", golden_shelleyNodeKeyGenKes_te)
        , ("golden_shelleyNodeKeyGenVrf", golden_shelleyNodeKeyGenVrf)
        , ("golden_shelleyNodeKeyGenVrf_bech32", golden_shelleyNodeKeyGenVrf_bech32)
        , ("golden_shelleyNodeKeyGenVrf_te", golden_shelleyNodeKeyGenVrf_te)
        , ("golden_shelleyPaymentKeys", golden_shelleyPaymentKeys)
        , ("golden_shelleyPaymentKeys_bech32", golden_shelleyPaymentKeys_bech32)
        , ("golden_shelleyPaymentKeys_te", golden_shelleyPaymentKeys_te)
        , ("golden_shelleyStakeAddressBuild", golden_shelleyStakeAddressBuild)
        , ("golden_shelleyStakeAddressDeregistrationCertificate", golden_shelleyStakeAddressDeregistrationCertificate)
        , ("golden_shelleyStakeAddressKeyGen", golden_shelleyStakeAddressKeyGen)
        , ("golden_shelleyStakeAddressRegistrationCertificate", golden_shelleyStakeAddressRegistrationCertificate)
        , ("golden_shelleyStakeKeys", golden_shelleyStakeKeys)
        , ("golden_shelleyStakeKeys_bech32", golden_shelleyStakeKeys_bech32)
        , ("golden_shelleyStakeKeys_te", golden_shelleyStakeKeys_te)
        , ("golden_shelleyStakePoolRegistrationCertificate", golden_shelleyStakePoolRegistrationCertificate)
        , ("golden_shelleyTextViewDecodeCbor", golden_shelleyTextViewDecodeCbor)
        , ("golden_shelleyTransactionBuild", golden_shelleyTransactionBuild)
        , ("golden_shelleyTransactionBuild_TxInScriptWitnessed", golden_shelleyTransactionBuild_TxInScriptWitnessed)
        , ("golden_shelleyTransactionBuild_Minting", golden_shelleyTransactionBuild_Minting)
        , ("golden_shelleyTransactionBuild_CertificateScriptWitnessed", golden_shelleyTransactionBuild_CertificateScriptWitnessed)
        , ("golden_shelleyTransactionBuild_WithdrawalScriptWitnessed", golden_shelleyTransactionBuild_WithdrawalScriptWitnessed)
        , ("golden_shelleyTransactionCalculateMinFee", golden_shelleyTransactionCalculateMinFee)
        , ("golden_shelleyTransactionSign", golden_shelleyTransactionSign)
        , ("golden_shelleyVRFKeys", golden_shelleyVRFKeys)
        , ("golden_shelleyVRFKeys_bech32", golden_shelleyVRFKeys_bech32)
        , ("golden_shelleyVRFKeys_te", golden_shelleyVRFKeys_te)
        , ("golden_version", golden_version)
        ]

txTests :: IO Bool
txTests =
  H.checkSequential
    $ H.Group "TextEnvelope Tx Goldens"
        [ ("golden_shelleyTxBody", golden_shelleyTxBody)
        , ("golden_shelleyTx", golden_shelleyTx)
        ]

certificateTests :: IO Bool
certificateTests =
  H.checkSequential
    $ H.Group "TextEnvelope Certificate Goldens"
        [ ("golden_shelleyStakeAddressCertificates", golden_shelleyStakeAddressCertificates)
        , ("golden_shelleyOperationalCertificate", golden_shelleyOperationalCertificate)
        , ("golden_shelleyStakePoolCertificates", golden_shelleyStakePoolCertificates)
        , ("golden_shelleyMIRCertificate", golden_shelleyMIRCertificate)
        , ("golden_shelleyGenesisKeyDelegationCertificate", golden_shelleyGenesisKeyDelegationCertificate)
        ]

keyConversionTests :: IO Bool
keyConversionTests =
  H.checkSequential
    $ H.Group "Key Conversion Goldens"
        [ ("golden_convertCardanoAddressByronSigningKey", golden_convertCardanoAddressByronSigningKey)
        , ("golden_convertCardanoAddressIcarusSigningKey", golden_convertCardanoAddressIcarusSigningKey)
        , ("golden_convertCardanoAddressShelleyPaymentSigningKey", golden_convertCardanoAddressShelleyPaymentSigningKey)
        , ("golden_convertCardanoAddressShelleyStakeSigningKey", golden_convertCardanoAddressShelleyStakeSigningKey)
        ]

metadataTests :: IO Bool
metadataTests =
  H.checkSequential
    $ H.Group "Metadata Goldens"
        [ ("golden_stakePoolMetadataHash", golden_stakePoolMetadataHash)
        ]

multiSigTests :: IO Bool
multiSigTests =
  H.checkSequential
    $ H.Group "Multisig Goldens"
        [ ("golden_shelleyAllMultiSigAddressBuild", golden_shelleyAllMultiSigAddressBuild)
        , ("golden_shelleyAnyMultiSigAddressBuild", golden_shelleyAnyMultiSigAddressBuild)
        , ("golden_shelleyAtLeastMultiSigAddressBuild", golden_shelleyAtLeastMultiSigAddressBuild)
        , ("golden_shelleyTransactionAssembleWitness_SigningKey", golden_shelleyTransactionAssembleWitness_SigningKey)
        , ("golden_shelleyTransactionSigningKeyWitness", golden_shelleyTransactionSigningKeyWitness)
        ]

governancePollTests :: IO Bool
governancePollTests =
  H.checkSequential
    $ H.Group "Governance Poll Goldens"
        [ ("golden_shelleyGovernanceCreatePoll", golden_shelleyGovernanceCreatePoll)
        , ("golden_shelleyGovernanceCreateLongPoll", golden_shelleyGovernanceCreateLongPoll)
        , ("golden_shelleyGovernanceAnswerPollNeg1Invalid", golden_shelleyGovernanceAnswerPollNeg1Invalid)
        , ("golden_shelleyGovernanceAnswerPoll0", golden_shelleyGovernanceAnswerPoll0)
        , ("golden_shelleyGovernanceAnswerPollPos1", golden_shelleyGovernanceAnswerPollPos1)
        , ("golden_shelleyGovernanceAnswerPollPos2Invalid", golden_shelleyGovernanceAnswerPollPos2Invalid)
        , ("golden_shelleyGovernanceVerifyPoll", golden_shelleyGovernanceVerifyPoll)
        , ("golden_shelleyGovernanceVerifyPoll (mismatch)", golden_shelleyGovernanceVerifyPollMismatch)
        , ("golden_shelleyGovernanceVerifyPoll (no answer)", golden_shelleyGovernanceVerifyPollNoAnswer)
        , ("golden_shelleyGovernanceVerifyPoll (malformed)", golden_shelleyGovernanceVerifyPollMalformedAnswer)
        , ("golden_shelleyGovernanceVerifyPoll (invalid)", golden_shelleyGovernanceVerifyPollInvalidAnswer)
        ]

