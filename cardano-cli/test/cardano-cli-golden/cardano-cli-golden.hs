{-# LANGUAGE FlexibleInstances #-}

module Main (main, ingredients, tests) where

import Prelude

import Data.String (fromString)
import System.Environment qualified as E

import Test.Golden.Byron.SigningKeys qualified
import Test.Golden.Byron.Tx qualified
import Test.Golden.Byron.TxBody qualified
import Test.Golden.Byron.UpdateProposal qualified
import Test.Golden.Byron.Vote qualified
import Test.Golden.Conway.Transaction.Assemble qualified
import Test.Golden.Conway.Transaction.BuildRaw qualified
import Test.Golden.Conway.Transaction.CreateWitness qualified
import Test.Golden.CreateStaked qualified
import Test.Golden.CreateTestnetData qualified
import Test.Golden.Governance.Action qualified
import Test.Golden.Governance.Committee qualified
import Test.Golden.Governance.DRep qualified
import Test.Golden.Governance.StakeAddress qualified
import Test.Golden.Governance.Vote qualified
import Test.Golden.Hash.Hash qualified
import Test.Golden.Help qualified
import Test.Golden.Key.NonExtendedKey qualified
import Test.Golden.Latest.Transaction.CalculateMinFee qualified
import Test.Golden.Shelley.Address.Build qualified
import Test.Golden.Shelley.Address.Info qualified
import Test.Golden.Shelley.Address.KeyGen qualified
import Test.Golden.Shelley.Genesis.InitialTxIn qualified
import Test.Golden.Shelley.Genesis.KeyGenDelegate qualified
import Test.Golden.Shelley.Genesis.KeyGenGenesis qualified
import Test.Golden.Shelley.Genesis.KeyGenUtxo qualified
import Test.Golden.Shelley.Genesis.KeyHash qualified
import Test.Golden.Shelley.Key.ConvertCardanoAddressKey qualified
import Test.Golden.Shelley.Metadata.StakePoolMetadata qualified
import Test.Golden.Shelley.MultiSig.Address qualified
import Test.Golden.Shelley.Node.IssueOpCert qualified
import Test.Golden.Shelley.Node.KeyGen qualified
import Test.Golden.Shelley.Node.KeyGenKes qualified
import Test.Golden.Shelley.Node.KeyGenVrf qualified
import Test.Golden.Shelley.StakeAddress.Build qualified
import Test.Golden.Shelley.StakeAddress.DeregistrationCertificate qualified
import Test.Golden.Shelley.StakeAddress.KeyGen qualified
import Test.Golden.Shelley.StakeAddress.KeyHash qualified
import Test.Golden.Shelley.StakeAddress.RegistrationCertificate qualified
import Test.Golden.Shelley.StakePool.RegistrationCertificate qualified
import Test.Golden.Shelley.TextEnvelope.Certificates.GenesisKeyDelegation qualified
import Test.Golden.Shelley.TextEnvelope.Certificates.Operational qualified
import Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys qualified
import Test.Golden.Shelley.TextEnvelope.Keys.GenesisDelegateKeys qualified
import Test.Golden.Shelley.TextEnvelope.Keys.GenesisKeys qualified
import Test.Golden.Shelley.TextEnvelope.Keys.GenesisUTxOKeys qualified
import Test.Golden.Shelley.TextEnvelope.Keys.KESKeys qualified
import Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys qualified
import Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys qualified
import Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys qualified
import Test.Golden.Shelley.TextEnvelope.Tx.Tx qualified
import Test.Golden.Shelley.TextEnvelope.Tx.Witness qualified
import Test.Golden.Shelley.TextView.DecodeCbor qualified
import Test.Golden.Shelley.Transaction.Assemble qualified
import Test.Golden.Shelley.Transaction.Build qualified
import Test.Golden.Shelley.Transaction.Id qualified
import Test.Golden.Shelley.Transaction.Sign qualified
import Test.Golden.TxView qualified
import Test.Golden.Version qualified
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as H
import Test.Tasty.Ingredients qualified as T

{- HLINT ignore "Use let" -}

tests :: IO T.TestTree
tests = do
  t0 <-
    pure $
      H.testPropertyNamed
        "deserialise legacy signing Key"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_deserialise_legacy_signing_Key")
        Test.Golden.Byron.SigningKeys.hprop_deserialise_legacy_signing_Key

  t1 <-
    pure $
      H.testPropertyNamed
        "deserialise nonLegacy signing Key"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_deserialise_nonLegacy_signing_Key")
        Test.Golden.Byron.SigningKeys.hprop_deserialise_nonLegacy_signing_Key

  t2 <-
    pure $
      H.testPropertyNamed
        "print legacy signing key address"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_print_legacy_signing_key_address")
        Test.Golden.Byron.SigningKeys.hprop_print_legacy_signing_key_address

  t3 <-
    pure $
      H.testPropertyNamed
        "print nonLegacy signing key address"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_print_nonLegacy_signing_key_address")
        Test.Golden.Byron.SigningKeys.hprop_print_nonLegacy_signing_key_address

  t4 <-
    pure $
      H.testPropertyNamed
        "generate and read nonlegacy signingkeys"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_generate_and_read_nonlegacy_signingkeys")
        Test.Golden.Byron.SigningKeys.hprop_generate_and_read_nonlegacy_signingkeys

  t5 <-
    pure $
      H.testPropertyNamed
        "migrate legacy to nonlegacy signingkeys"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_migrate_legacy_to_nonlegacy_signingkeys")
        Test.Golden.Byron.SigningKeys.hprop_migrate_legacy_to_nonlegacy_signingkeys

  t6 <-
    pure $
      H.testPropertyNamed
        "deserialise NonLegacy Signing Key API"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_deserialise_NonLegacy_Signing_Key_API")
        Test.Golden.Byron.SigningKeys.hprop_deserialise_NonLegacy_Signing_Key_API

  t7 <-
    pure $
      H.testPropertyNamed
        "deserialiseLegacy Signing Key API"
        (fromString "Test.Golden.Byron.SigningKeys.hprop_deserialiseLegacy_Signing_Key_API")
        Test.Golden.Byron.SigningKeys.hprop_deserialiseLegacy_Signing_Key_API

  t8 <-
    pure $
      H.testPropertyNamed
        "byronTx legacy"
        (fromString "Test.Golden.Byron.Tx.hprop_byronTx_legacy")
        Test.Golden.Byron.Tx.hprop_byronTx_legacy

  t9 <-
    pure $
      H.testPropertyNamed
        "byronTx"
        (fromString "Test.Golden.Byron.Tx.hprop_byronTx")
        Test.Golden.Byron.Tx.hprop_byronTx

  t10 <-
    pure $
      H.testPropertyNamed
        "golden byronTxBody"
        (fromString "Test.Golden.Byron.TxBody.hprop_golden_byronTxBody")
        Test.Golden.Byron.TxBody.hprop_golden_byronTxBody

  t11 <-
    pure $
      H.testPropertyNamed
        "byron update proposal"
        (fromString "Test.Golden.Byron.UpdateProposal.hprop_byron_update_proposal")
        Test.Golden.Byron.UpdateProposal.hprop_byron_update_proposal

  t12 <-
    pure $
      H.testPropertyNamed
        "byron yes vote"
        (fromString "Test.Golden.Byron.Vote.hprop_byron_yes_vote")
        Test.Golden.Byron.Vote.hprop_byron_yes_vote

  t13 <-
    pure $
      H.testPropertyNamed
        "byron no vote"
        (fromString "Test.Golden.Byron.Vote.hprop_byron_no_vote")
        Test.Golden.Byron.Vote.hprop_byron_no_vote

  t14 <-
    pure $
      H.testPropertyNamed
        "golden conway transaction assemble witness signing key"
        ( fromString
            "Test.Golden.Conway.Transaction.Assemble.hprop_golden_conway_transaction_assemble_witness_signing_key"
        )
        Test.Golden.Conway.Transaction.Assemble.hprop_golden_conway_transaction_assemble_witness_signing_key

  t15 <-
    pure $
      H.testPropertyNamed
        "golden conway build raw treasury donation"
        (fromString "Test.Golden.Conway.Transaction.BuildRaw.hprop_golden_conway_build_raw_treasury_donation")
        Test.Golden.Conway.Transaction.BuildRaw.hprop_golden_conway_build_raw_treasury_donation

  t16 <-
    pure $
      H.testPropertyNamed
        "golden conway build raw donation no current treasury value"
        ( fromString
            "Test.Golden.Conway.Transaction.BuildRaw.hprop_golden_conway_build_raw_donation_no_current_treasury_value"
        )
        Test.Golden.Conway.Transaction.BuildRaw.hprop_golden_conway_build_raw_donation_no_current_treasury_value

  t17 <-
    pure $
      H.testPropertyNamed
        "golden conway build raw donation no treasury donation"
        ( fromString
            "Test.Golden.Conway.Transaction.BuildRaw.hprop_golden_conway_build_raw_donation_no_treasury_donation"
        )
        Test.Golden.Conway.Transaction.BuildRaw.hprop_golden_conway_build_raw_donation_no_treasury_donation

  t18 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction signing key witness"
        ( fromString
            "Test.Golden.Conway.Transaction.CreateWitness.hprop_golden_shelley_transaction_signing_key_witness"
        )
        Test.Golden.Conway.Transaction.CreateWitness.hprop_golden_shelley_transaction_signing_key_witness

  t19 <-
    pure $
      H.testPropertyNamed
        "golden create staked"
        (fromString "Test.Golden.CreateStaked.hprop_golden_create_staked")
        Test.Golden.CreateStaked.hprop_golden_create_staked

  t20 <-
    pure $
      H.testPropertyNamed
        "golden create testnet data"
        (fromString "Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data")
        Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data

  t21 <-
    pure $
      H.testPropertyNamed
        "golden create testnet data with template"
        (fromString "Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data_with_template")
        Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data_with_template

  t22 <-
    pure $
      H.testPropertyNamed
        "golden create testnet data deleg non deleg"
        (fromString "Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data_deleg_non_deleg")
        Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data_deleg_non_deleg

  t23 <-
    pure $
      H.testPropertyNamed
        "golden create testnet data shelley genesis output"
        (fromString "Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data_shelley_genesis_output")
        Test.Golden.CreateTestnetData.hprop_golden_create_testnet_data_shelley_genesis_output

  t29 <-
    pure $
      H.testPropertyNamed
        "golden governance action create constitution wrong hash1 fails"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_governance_action_create_constitution_wrong_hash1_fails"
        )
        Test.Golden.Governance.Action.hprop_golden_governance_action_create_constitution_wrong_hash1_fails

  t30 <-
    pure $
      H.testPropertyNamed
        "golden governance action create constitution wrong hash2 fails"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_governance_action_create_constitution_wrong_hash2_fails"
        )
        Test.Golden.Governance.Action.hprop_golden_governance_action_create_constitution_wrong_hash2_fails

  t31 <-
    pure $
      H.testPropertyNamed
        "golden governance action create constitution"
        (fromString "Test.Golden.Governance.Action.hprop_golden_governance_action_create_constitution")
        Test.Golden.Governance.Action.hprop_golden_governance_action_create_constitution

  t32 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action view constitution json"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_constitution_json"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_constitution_json

  t33 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action view update committee yaml wrong hash fails"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_update_committee_yaml_wrong_hash_fails"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_update_committee_yaml_wrong_hash_fails

  t34 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action view update committee yaml"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_update_committee_yaml"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_update_committee_yaml

  t35 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action view create info json outfile wrong hash fails"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_create_info_json_outfile_wrong_hash_fails"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_create_info_json_outfile_wrong_hash_fails

  t36 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action view create info json outfile"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_create_info_json_outfile"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_view_create_info_json_outfile

  t37 <-
    pure $
      H.testPropertyNamed
        "golden governanceActionCreateNoConfidence wrong hash fails"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_governanceActionCreateNoConfidence_wrong_hash_fails"
        )
        Test.Golden.Governance.Action.hprop_golden_governanceActionCreateNoConfidence_wrong_hash_fails

  t38 <-
    pure $
      H.testPropertyNamed
        "golden governanceActionCreateNoConfidence"
        (fromString "Test.Golden.Governance.Action.hprop_golden_governanceActionCreateNoConfidence")
        Test.Golden.Governance.Action.hprop_golden_governanceActionCreateNoConfidence

  t39 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action create protocol parameters update wrong hash fails"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_protocol_parameters_update_wrong_hash_fails"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_protocol_parameters_update_wrong_hash_fails

  t40 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action create protocol parameters update"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_protocol_parameters_update"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_protocol_parameters_update

  t41 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action create protocol parameters update partial costmodel"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_protocol_parameters_update_partial_costmodel"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_protocol_parameters_update_partial_costmodel

  t42 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action create hardfork wrong hash fails"
        ( fromString
            "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_hardfork_wrong_hash_fails"
        )
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_hardfork_wrong_hash_fails

  t43 <-
    pure $
      H.testPropertyNamed
        "golden conway governance action create hardfork"
        (fromString "Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_hardfork")
        Test.Golden.Governance.Action.hprop_golden_conway_governance_action_create_hardfork

  t44 <-
    pure $
      H.testPropertyNamed
        "golden governance committee key gen"
        (fromString "Test.Golden.Governance.Committee.hprop_golden_governance_committee_key_gen")
        Test.Golden.Governance.Committee.hprop_golden_governance_committee_key_gen

  t45 <-
    pure $
      H.testPropertyNamed
        "golden governance CommitteeCreateHotKeyAuthorizationCertificate"
        ( fromString
            "Test.Golden.Governance.Committee.hprop_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate"
        )
        Test.Golden.Governance.Committee.hprop_golden_governance_CommitteeCreateHotKeyAuthorizationCertificate

  t46 <-
    pure $
      H.testPropertyNamed
        "golden governance CommitteeCreateColdKeyResignationCertificate"
        ( fromString
            "Test.Golden.Governance.Committee.hprop_golden_governance_CommitteeCreateColdKeyResignationCertificate"
        )
        Test.Golden.Governance.Committee.hprop_golden_governance_CommitteeCreateColdKeyResignationCertificate

  t47 <-
    pure $
      H.testPropertyNamed
        "golden governance UpdateCommittee"
        (fromString "Test.Golden.Governance.Committee.hprop_golden_governance_UpdateCommittee")
        Test.Golden.Governance.Committee.hprop_golden_governance_UpdateCommittee

  t48 <-
    pure $
      H.testPropertyNamed
        "golden governance committee cold extended key signing"
        ( fromString
            "Test.Golden.Governance.Committee.hprop_golden_governance_committee_cold_extended_key_signing"
        )
        Test.Golden.Governance.Committee.hprop_golden_governance_committee_cold_extended_key_signing

  t49 <-
    pure $
      H.testPropertyNamed
        "golden governance committee hot extended key signing"
        ( fromString
            "Test.Golden.Governance.Committee.hprop_golden_governance_committee_hot_extended_key_signing"
        )
        Test.Golden.Governance.Committee.hprop_golden_governance_committee_hot_extended_key_signing

  t50 <-
    pure $
      H.testPropertyNamed
        "golden verification key committee"
        (fromString "Test.Golden.Governance.Committee.hprop_golden_verification_key_committee")
        Test.Golden.Governance.Committee.hprop_golden_verification_key_committee

  t51 <-
    pure $
      H.testPropertyNamed
        "golden governance extended committee key hash"
        (fromString "Test.Golden.Governance.Committee.hprop_golden_governance_extended_committee_key_hash")
        Test.Golden.Governance.Committee.hprop_golden_governance_extended_committee_key_hash

  t52 <-
    pure $
      H.testPropertyNamed
        "golden governance committee checks wrong hash fails"
        ( fromString
            "Test.Golden.Governance.Committee.hprop_golden_governance_committee_checks_wrong_hash_fails"
        )
        Test.Golden.Governance.Committee.hprop_golden_governance_committee_checks_wrong_hash_fails

  t53 <-
    pure $
      H.testPropertyNamed
        "golden governanceDRepKeyGen"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governanceDRepKeyGen")
        Test.Golden.Governance.DRep.hprop_golden_governanceDRepKeyGen

  t54 <-
    pure $
      H.testPropertyNamed
        "golden governance drep id bech32"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_id_bech32")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_id_bech32

  t55 <-
    pure $
      H.testPropertyNamed
        "golden governance drep id hex"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_id_hex")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_id_hex

  t56 <-
    pure $
      H.testPropertyNamed
        "golden governance drep id hash"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_id_hash")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_id_hash

  t57 <-
    pure $
      H.testPropertyNamed
        "golden governance drep extended key signing"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_extended_key_signing")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_extended_key_signing

  t58 <-
    pure $
      H.testPropertyNamed
        "golden governance drep retirement certificate vkey file"
        ( fromString
            "Test.Golden.Governance.DRep.hprop_golden_governance_drep_retirement_certificate_vkey_file"
        )
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_retirement_certificate_vkey_file

  t59 <-
    pure $
      H.testPropertyNamed
        "golden governance drep retirement certificate id hex"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_retirement_certificate_id_hex")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_retirement_certificate_id_hex

  t60 <-
    pure $
      H.testPropertyNamed
        "golden governance drep retirement certificate id bech32"
        ( fromString
            "Test.Golden.Governance.DRep.hprop_golden_governance_drep_retirement_certificate_id_bech32"
        )
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_retirement_certificate_id_bech32

  t61 <-
    pure $
      H.testPropertyNamed
        "golden governance drep metadata hash"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_metadata_hash")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_metadata_hash

  t62 <-
    pure $
      H.testPropertyNamed
        "golden governance drep metadata hash cip119"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_metadata_hash_cip119")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_metadata_hash_cip119

  t63 <-
    pure $
      H.testPropertyNamed
        "golden governance drep registration certificate vkey file"
        ( fromString
            "Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_vkey_file"
        )
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_vkey_file

  t64 <-
    pure $
      H.testPropertyNamed
        "golden governance drep registration certificate id hex"
        ( fromString
            "Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_id_hex"
        )
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_id_hex

  t65 <-
    pure $
      H.testPropertyNamed
        "golden governance drep registration certificate id bech32"
        ( fromString
            "Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_id_bech32"
        )
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_id_bech32

  t66 <-
    pure $
      H.testPropertyNamed
        "golden governance drep registration certificate script hash"
        ( fromString
            "Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_script_hash"
        )
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_registration_certificate_script_hash

  t67 <-
    pure $
      H.testPropertyNamed
        "golden governance drep update certificate vkey file"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_update_certificate_vkey_file")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_update_certificate_vkey_file

  t68 <-
    pure $
      H.testPropertyNamed
        "golden governance drep update certificate script hash"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_governance_drep_update_certificate_script_hash")
        Test.Golden.Governance.DRep.hprop_golden_governance_drep_update_certificate_script_hash

  t69 <-
    pure $
      H.testPropertyNamed
        "golden verification key drep"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_verification_key_drep")
        Test.Golden.Governance.DRep.hprop_golden_verification_key_drep

  t70 <-
    pure $
      H.testPropertyNamed
        "golden drep metadata hash url wrong hash fails"
        (fromString "Test.Golden.Governance.DRep.hprop_golden_drep_metadata_hash_url_wrong_hash_fails")
        Test.Golden.Governance.DRep.hprop_golden_drep_metadata_hash_url_wrong_hash_fails

  t71 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress delegate no confidence"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_no_confidence"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_no_confidence

  t72 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress delegate always abstain"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_always_abstain"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_always_abstain

  t73 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress delegate pool and no confidence"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_pool_and_no_confidence"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_pool_and_no_confidence

  t74 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress delegate pool and always abstain"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_pool_and_always_abstain"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_pool_and_always_abstain

  t75 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress delegate pool and drep"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_pool_and_drep"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_delegate_pool_and_drep

  t76 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress register and delegate pool"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_register_and_delegate_pool"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_register_and_delegate_pool

  t77 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress register and delegate vote"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_register_and_delegate_vote"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_register_and_delegate_vote

  t78 <-
    pure $
      H.testPropertyNamed
        "golden conway stakeaddress register and delegate stake and vote"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_register_and_delegate_stake_and_vote"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_conway_stakeaddress_register_and_delegate_stake_and_vote

  t79 <-
    pure $
      H.testPropertyNamed
        "golden stake pool metadata hash url wrong hash"
        ( fromString
            "Test.Golden.Governance.StakeAddress.hprop_golden_stake_pool_metadata_hash_url_wrong_hash"
        )
        Test.Golden.Governance.StakeAddress.hprop_golden_stake_pool_metadata_hash_url_wrong_hash

  t80 <-
    pure $
      H.testPropertyNamed
        "golden governance governance vote create"
        (fromString "Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create")
        Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create

  t81 <-
    pure $
      H.testPropertyNamed
        "golden governance governance vote view json stdout"
        (fromString "Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_view_json_stdout")
        Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_view_json_stdout

  t82 <-
    pure $
      H.testPropertyNamed
        "golden governance governance vote view json outfile"
        (fromString "Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_view_json_outfile")
        Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_view_json_outfile

  t83 <-
    pure $
      H.testPropertyNamed
        "golden governance governance vote view yaml"
        (fromString "Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_view_yaml")
        Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_view_yaml

  t84 <-
    pure $
      H.testPropertyNamed
        "golden governance governance vote create yes cc hot key"
        ( fromString
            "Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create_yes_cc_hot_key"
        )
        Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create_yes_cc_hot_key

  t85 <-
    pure $
      H.testPropertyNamed
        "golden governance governance vote create no cc hot key"
        ( fromString
            "Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create_no_cc_hot_key"
        )
        Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create_no_cc_hot_key

  t86 <-
    pure $
      H.testPropertyNamed
        "golden governance governance vote create abstain cc hot key"
        ( fromString
            "Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create_abstain_cc_hot_key"
        )
        Test.Golden.Governance.Vote.hprop_golden_governance_governance_vote_create_abstain_cc_hot_key

  t87 <-
    pure $
      H.testPropertyNamed
        "golden governance vote create hash fails"
        (fromString "Test.Golden.Governance.Vote.hprop_golden_governance_vote_create_hash_fails")
        Test.Golden.Governance.Vote.hprop_golden_governance_vote_create_hash_fails

  t88 <-
    pure $
      H.testPropertyNamed
        "golden governance hash script"
        (fromString "Test.Golden.Hash.Hash.hprop_golden_governance_hash_script")
        Test.Golden.Hash.Hash.hprop_golden_governance_hash_script

  t89 <-
    pure $
      H.testPropertyNamed
        "golden HelpAll"
        (fromString "Test.Golden.Help.hprop_golden_HelpAll")
        Test.Golden.Help.hprop_golden_HelpAll

  t91 <-
    pure $
      H.testPropertyNamed
        "golden KeyNonExtendedKey GenesisExtendedVerificationKey"
        ( fromString
            "Test.Golden.Key.NonExtendedKey.hprop_golden_KeyNonExtendedKey_GenesisExtendedVerificationKey"
        )
        Test.Golden.Key.NonExtendedKey.hprop_golden_KeyNonExtendedKey_GenesisExtendedVerificationKey

  t92 <-
    pure $
      H.testPropertyNamed
        "golden KeyNonExtendedKey StakeExtendedVerificationKeyShelley"
        ( fromString
            "Test.Golden.Key.NonExtendedKey.hprop_golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley"
        )
        Test.Golden.Key.NonExtendedKey.hprop_golden_KeyNonExtendedKey_StakeExtendedVerificationKeyShelley

  t93 <-
    pure $
      H.testPropertyNamed
        "golden KeyNonExtendedKey DRepExtendedVerificationKey"
        ( fromString
            "Test.Golden.Key.NonExtendedKey.hprop_golden_KeyNonExtendedKey_DRepExtendedVerificationKey"
        )
        Test.Golden.Key.NonExtendedKey.hprop_golden_KeyNonExtendedKey_DRepExtendedVerificationKey

  t94 <-
    pure $
      H.testPropertyNamed
        "golden extended payment vkey to non extended vkey"
        (fromString "Test.Golden.Key.NonExtendedKey.hprop_golden_extended_payment_vkey_to_non_extended_vkey")
        Test.Golden.Key.NonExtendedKey.hprop_golden_extended_payment_vkey_to_non_extended_vkey

  t95 <-
    pure $
      H.testPropertyNamed
        "golden extended cc vkey to non extended vkey"
        (fromString "Test.Golden.Key.NonExtendedKey.hprop_golden_extended_cc_vkey_to_non_extended_vkey")
        Test.Golden.Key.NonExtendedKey.hprop_golden_extended_cc_vkey_to_non_extended_vkey

  t96 <-
    pure $
      H.testPropertyNamed
        "golden latest transaction calculate min fee"
        ( fromString
            "Test.Golden.Latest.Transaction.CalculateMinFee.hprop_golden_latest_transaction_calculate_min_fee"
        )
        Test.Golden.Latest.Transaction.CalculateMinFee.hprop_golden_latest_transaction_calculate_min_fee

  t97 <-
    pure $
      H.testPropertyNamed
        "golden latest transaction calculate min fee flags"
        ( fromString
            "Test.Golden.Latest.Transaction.CalculateMinFee.hprop_golden_latest_transaction_calculate_min_fee_flags"
        )
        Test.Golden.Latest.Transaction.CalculateMinFee.hprop_golden_latest_transaction_calculate_min_fee_flags

  t98 <-
    pure $
      H.testPropertyNamed
        "golden shelleyAddressBuild"
        (fromString "Test.Golden.Shelley.Address.Build.hprop_golden_shelleyAddressBuild")
        Test.Golden.Shelley.Address.Build.hprop_golden_shelleyAddressBuild

  t99 <-
    pure $
      H.testPropertyNamed
        "golden shelleyAddressInfo"
        (fromString "Test.Golden.Shelley.Address.Info.hprop_golden_shelleyAddressInfo")
        Test.Golden.Shelley.Address.Info.hprop_golden_shelleyAddressInfo

  t100 <-
    pure $
      H.testPropertyNamed
        "golden shelley address key gen"
        (fromString "Test.Golden.Shelley.Address.KeyGen.hprop_golden_shelley_address_key_gen")
        Test.Golden.Shelley.Address.KeyGen.hprop_golden_shelley_address_key_gen

  t101 <-
    pure $
      H.testPropertyNamed
        "golden shelley address extended key gen"
        (fromString "Test.Golden.Shelley.Address.KeyGen.hprop_golden_shelley_address_extended_key_gen")
        Test.Golden.Shelley.Address.KeyGen.hprop_golden_shelley_address_extended_key_gen

  t102 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisInitialTxIn"
        (fromString "Test.Golden.Shelley.Genesis.InitialTxIn.hprop_golden_shelleyGenesisInitialTxIn")
        Test.Golden.Shelley.Genesis.InitialTxIn.hprop_golden_shelleyGenesisInitialTxIn

  t103 <-
    pure $
      H.testPropertyNamed
        "golden shelley genesis key gen delegate"
        ( fromString
            "Test.Golden.Shelley.Genesis.KeyGenDelegate.hprop_golden_shelley_genesis_key_gen_delegate"
        )
        Test.Golden.Shelley.Genesis.KeyGenDelegate.hprop_golden_shelley_genesis_key_gen_delegate

  t104 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisKeyGenGenesis"
        (fromString "Test.Golden.Shelley.Genesis.KeyGenGenesis.hprop_golden_shelleyGenesisKeyGenGenesis")
        Test.Golden.Shelley.Genesis.KeyGenGenesis.hprop_golden_shelleyGenesisKeyGenGenesis

  t105 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisKeyGenUtxo"
        (fromString "Test.Golden.Shelley.Genesis.KeyGenUtxo.hprop_golden_shelleyGenesisKeyGenUtxo")
        Test.Golden.Shelley.Genesis.KeyGenUtxo.hprop_golden_shelleyGenesisKeyGenUtxo

  t106 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisKeyHash"
        (fromString "Test.Golden.Shelley.Genesis.KeyHash.hprop_golden_shelleyGenesisKeyHash")
        Test.Golden.Shelley.Genesis.KeyHash.hprop_golden_shelleyGenesisKeyHash

  t107 <-
    pure $
      H.testPropertyNamed
        "golden convertCardanoAddressByronSigningKey"
        ( fromString
            "Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressByronSigningKey"
        )
        Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressByronSigningKey

  t108 <-
    pure $
      H.testPropertyNamed
        "golden convertCardanoAddressIcarusSigningKey"
        ( fromString
            "Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressIcarusSigningKey"
        )
        Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressIcarusSigningKey

  t109 <-
    pure $
      H.testPropertyNamed
        "golden convertCardanoAddressShelleyPaymentSigningKey"
        ( fromString
            "Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressShelleyPaymentSigningKey"
        )
        Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressShelleyPaymentSigningKey

  t110 <-
    pure $
      H.testPropertyNamed
        "golden convertCardanoAddressShelleyStakeSigningKey"
        ( fromString
            "Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressShelleyStakeSigningKey"
        )
        Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convertCardanoAddressShelleyStakeSigningKey

  t111 <-
    pure $
      H.testPropertyNamed
        "golden convert cardano address cc drep"
        ( fromString
            "Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convert_cardano_address_cc_drep"
        )
        Test.Golden.Shelley.Key.ConvertCardanoAddressKey.hprop_golden_convert_cardano_address_cc_drep

  t112 <-
    pure $
      H.testPropertyNamed
        "golden stakePoolMetadataHash"
        (fromString "Test.Golden.Shelley.Metadata.StakePoolMetadata.hprop_golden_stakePoolMetadataHash")
        Test.Golden.Shelley.Metadata.StakePoolMetadata.hprop_golden_stakePoolMetadataHash

  t113 <-
    pure $
      H.testPropertyNamed
        "golden shelleyAllMultiSigAddressBuild"
        (fromString "Test.Golden.Shelley.MultiSig.Address.hprop_golden_shelleyAllMultiSigAddressBuild")
        Test.Golden.Shelley.MultiSig.Address.hprop_golden_shelleyAllMultiSigAddressBuild

  t114 <-
    pure $
      H.testPropertyNamed
        "golden shelleyAnyMultiSigAddressBuild"
        (fromString "Test.Golden.Shelley.MultiSig.Address.hprop_golden_shelleyAnyMultiSigAddressBuild")
        Test.Golden.Shelley.MultiSig.Address.hprop_golden_shelleyAnyMultiSigAddressBuild

  t115 <-
    pure $
      H.testPropertyNamed
        "golden shelleyAtLeastMultiSigAddressBuild"
        (fromString "Test.Golden.Shelley.MultiSig.Address.hprop_golden_shelleyAtLeastMultiSigAddressBuild")
        Test.Golden.Shelley.MultiSig.Address.hprop_golden_shelleyAtLeastMultiSigAddressBuild

  t116 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeIssueOpCert"
        (fromString "Test.Golden.Shelley.Node.IssueOpCert.hprop_golden_shelleyNodeIssueOpCert")
        Test.Golden.Shelley.Node.IssueOpCert.hprop_golden_shelleyNodeIssueOpCert

  t117 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGen"
        (fromString "Test.Golden.Shelley.Node.KeyGen.hprop_golden_shelleyNodeKeyGen")
        Test.Golden.Shelley.Node.KeyGen.hprop_golden_shelleyNodeKeyGen

  t118 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGen te"
        (fromString "Test.Golden.Shelley.Node.KeyGen.hprop_golden_shelleyNodeKeyGen_te")
        Test.Golden.Shelley.Node.KeyGen.hprop_golden_shelleyNodeKeyGen_te

  t119 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGen bech32"
        (fromString "Test.Golden.Shelley.Node.KeyGen.hprop_golden_shelleyNodeKeyGen_bech32")
        Test.Golden.Shelley.Node.KeyGen.hprop_golden_shelleyNodeKeyGen_bech32

  t120 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGenKes"
        (fromString "Test.Golden.Shelley.Node.KeyGenKes.hprop_golden_shelleyNodeKeyGenKes")
        Test.Golden.Shelley.Node.KeyGenKes.hprop_golden_shelleyNodeKeyGenKes

  t121 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGenKes te"
        (fromString "Test.Golden.Shelley.Node.KeyGenKes.hprop_golden_shelleyNodeKeyGenKes_te")
        Test.Golden.Shelley.Node.KeyGenKes.hprop_golden_shelleyNodeKeyGenKes_te

  t122 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGenKes bech32"
        (fromString "Test.Golden.Shelley.Node.KeyGenKes.hprop_golden_shelleyNodeKeyGenKes_bech32")
        Test.Golden.Shelley.Node.KeyGenKes.hprop_golden_shelleyNodeKeyGenKes_bech32

  t123 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGenVrf"
        (fromString "Test.Golden.Shelley.Node.KeyGenVrf.hprop_golden_shelleyNodeKeyGenVrf")
        Test.Golden.Shelley.Node.KeyGenVrf.hprop_golden_shelleyNodeKeyGenVrf

  t124 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGenVrf te"
        (fromString "Test.Golden.Shelley.Node.KeyGenVrf.hprop_golden_shelleyNodeKeyGenVrf_te")
        Test.Golden.Shelley.Node.KeyGenVrf.hprop_golden_shelleyNodeKeyGenVrf_te

  t125 <-
    pure $
      H.testPropertyNamed
        "golden shelleyNodeKeyGenVrf bech32"
        (fromString "Test.Golden.Shelley.Node.KeyGenVrf.hprop_golden_shelleyNodeKeyGenVrf_bech32")
        Test.Golden.Shelley.Node.KeyGenVrf.hprop_golden_shelleyNodeKeyGenVrf_bech32

  t126 <-
    pure $
      H.testPropertyNamed
        "golden shelleyStakeAddressBuild"
        (fromString "Test.Golden.Shelley.StakeAddress.Build.hprop_golden_shelleyStakeAddressBuild")
        Test.Golden.Shelley.StakeAddress.Build.hprop_golden_shelleyStakeAddressBuild

  t127 <-
    pure $
      H.testPropertyNamed
        "golden shelley stake address deregistration certificate"
        ( fromString
            "Test.Golden.Shelley.StakeAddress.DeregistrationCertificate.hprop_golden_shelley_stake_address_deregistration_certificate"
        )
        Test.Golden.Shelley.StakeAddress.DeregistrationCertificate.hprop_golden_shelley_stake_address_deregistration_certificate

  t128 <-
    pure $
      H.testPropertyNamed
        "golden shelleyStakeAddressKeyGen"
        (fromString "Test.Golden.Shelley.StakeAddress.KeyGen.hprop_golden_shelleyStakeAddressKeyGen")
        Test.Golden.Shelley.StakeAddress.KeyGen.hprop_golden_shelleyStakeAddressKeyGen

  t129 <-
    pure $
      H.testPropertyNamed
        "golden shelleyStakeAddressKeyHash"
        (fromString "Test.Golden.Shelley.StakeAddress.KeyHash.hprop_golden_shelleyStakeAddressKeyHash")
        Test.Golden.Shelley.StakeAddress.KeyHash.hprop_golden_shelleyStakeAddressKeyHash

  t130 <-
    pure $
      H.testPropertyNamed
        "golden shelley stake address registration certificate"
        ( fromString
            "Test.Golden.Shelley.StakeAddress.RegistrationCertificate.hprop_golden_shelley_stake_address_registration_certificate"
        )
        Test.Golden.Shelley.StakeAddress.RegistrationCertificate.hprop_golden_shelley_stake_address_registration_certificate

  t131 <-
    pure $
      H.testPropertyNamed
        "golden shelley stake address registration certificate with build raw"
        ( fromString
            "Test.Golden.Shelley.StakeAddress.RegistrationCertificate.hprop_golden_shelley_stake_address_registration_certificate_with_build_raw"
        )
        Test.Golden.Shelley.StakeAddress.RegistrationCertificate.hprop_golden_shelley_stake_address_registration_certificate_with_build_raw

  t132 <-
    pure $
      H.testPropertyNamed
        "golden shelley stake address registration certificate missing reg deposit"
        ( fromString
            "Test.Golden.Shelley.StakeAddress.RegistrationCertificate.hprop_golden_shelley_stake_address_registration_certificate_missing_reg_deposit"
        )
        Test.Golden.Shelley.StakeAddress.RegistrationCertificate.hprop_golden_shelley_stake_address_registration_certificate_missing_reg_deposit

  t133 <-
    pure $
      H.testPropertyNamed
        "golden shelley stake pool registration certificate"
        ( fromString
            "Test.Golden.Shelley.StakePool.RegistrationCertificate.hprop_golden_shelley_stake_pool_registration_certificate"
        )
        Test.Golden.Shelley.StakePool.RegistrationCertificate.hprop_golden_shelley_stake_pool_registration_certificate

  t134 <-
    pure $
      H.testPropertyNamed
        "golden conway stake pool registration certificate extended cold key"
        ( fromString
            "Test.Golden.Shelley.StakePool.RegistrationCertificate.hprop_golden_conway_stake_pool_registration_certificate_extended_cold_key"
        )
        Test.Golden.Shelley.StakePool.RegistrationCertificate.hprop_golden_conway_stake_pool_registration_certificate_extended_cold_key

  t135 <-
    pure $
      H.testPropertyNamed
        "golden conway stake pool registration certificate extended literal cold key"
        ( fromString
            "Test.Golden.Shelley.StakePool.RegistrationCertificate.hprop_golden_conway_stake_pool_registration_certificate_extended_literal_cold_key"
        )
        Test.Golden.Shelley.StakePool.RegistrationCertificate.hprop_golden_conway_stake_pool_registration_certificate_extended_literal_cold_key

  t136 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisKeyDelegationCertificate"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Certificates.GenesisKeyDelegation.hprop_golden_shelleyGenesisKeyDelegationCertificate"
        )
        Test.Golden.Shelley.TextEnvelope.Certificates.GenesisKeyDelegation.hprop_golden_shelleyGenesisKeyDelegationCertificate

  t137 <-
    pure $
      H.testPropertyNamed
        "golden shelleyOperationalCertificate"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Certificates.Operational.hprop_golden_shelleyOperationalCertificate"
        )
        Test.Golden.Shelley.TextEnvelope.Certificates.Operational.hprop_golden_shelleyOperationalCertificate

  t138 <-
    pure $
      H.testPropertyNamed
        "golden shelleyExtendedPaymentKeys"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys.hprop_golden_shelleyExtendedPaymentKeys"
        )
        Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys.hprop_golden_shelleyExtendedPaymentKeys

  t139 <-
    pure $
      H.testPropertyNamed
        "golden shelleyExtendedPaymentKeys te"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys.hprop_golden_shelleyExtendedPaymentKeys_te"
        )
        Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys.hprop_golden_shelleyExtendedPaymentKeys_te

  t140 <-
    pure $
      H.testPropertyNamed
        "golden shelleyExtendedPaymentKeys bech32"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys.hprop_golden_shelleyExtendedPaymentKeys_bech32"
        )
        Test.Golden.Shelley.TextEnvelope.Keys.ExtendedPaymentKeys.hprop_golden_shelleyExtendedPaymentKeys_bech32

  t141 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisDelegateKeys"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Keys.GenesisDelegateKeys.hprop_golden_shelleyGenesisDelegateKeys"
        )
        Test.Golden.Shelley.TextEnvelope.Keys.GenesisDelegateKeys.hprop_golden_shelleyGenesisDelegateKeys

  t142 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisKeys"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.GenesisKeys.hprop_golden_shelleyGenesisKeys")
        Test.Golden.Shelley.TextEnvelope.Keys.GenesisKeys.hprop_golden_shelleyGenesisKeys

  t143 <-
    pure $
      H.testPropertyNamed
        "golden shelleyGenesisUTxOKeys"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Keys.GenesisUTxOKeys.hprop_golden_shelleyGenesisUTxOKeys"
        )
        Test.Golden.Shelley.TextEnvelope.Keys.GenesisUTxOKeys.hprop_golden_shelleyGenesisUTxOKeys

  t144 <-
    pure $
      H.testPropertyNamed
        "golden shelleyKESKeys"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.KESKeys.hprop_golden_shelleyKESKeys")
        Test.Golden.Shelley.TextEnvelope.Keys.KESKeys.hprop_golden_shelleyKESKeys

  t145 <-
    pure $
      H.testPropertyNamed
        "golden shelleyKESKeys te"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.KESKeys.hprop_golden_shelleyKESKeys_te")
        Test.Golden.Shelley.TextEnvelope.Keys.KESKeys.hprop_golden_shelleyKESKeys_te

  t146 <-
    pure $
      H.testPropertyNamed
        "golden shelleyKESKeys bech32"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.KESKeys.hprop_golden_shelleyKESKeys_bech32")
        Test.Golden.Shelley.TextEnvelope.Keys.KESKeys.hprop_golden_shelleyKESKeys_bech32

  t147 <-
    pure $
      H.testPropertyNamed
        "golden shelleyPaymentKeys"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys.hprop_golden_shelleyPaymentKeys")
        Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys.hprop_golden_shelleyPaymentKeys

  t148 <-
    pure $
      H.testPropertyNamed
        "golden shelleyPaymentKeys te"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys.hprop_golden_shelleyPaymentKeys_te")
        Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys.hprop_golden_shelleyPaymentKeys_te

  t149 <-
    pure $
      H.testPropertyNamed
        "golden shelleyPaymentKeys bech32"
        ( fromString
            "Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys.hprop_golden_shelleyPaymentKeys_bech32"
        )
        Test.Golden.Shelley.TextEnvelope.Keys.PaymentKeys.hprop_golden_shelleyPaymentKeys_bech32

  t150 <-
    pure $
      H.testPropertyNamed
        "golden shelleyStakeKeys"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys.hprop_golden_shelleyStakeKeys")
        Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys.hprop_golden_shelleyStakeKeys

  t151 <-
    pure $
      H.testPropertyNamed
        "golden shelleyStakeKeys te"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys.hprop_golden_shelleyStakeKeys_te")
        Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys.hprop_golden_shelleyStakeKeys_te

  t152 <-
    pure $
      H.testPropertyNamed
        "golden shelleyStakeKeys bech32"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys.hprop_golden_shelleyStakeKeys_bech32")
        Test.Golden.Shelley.TextEnvelope.Keys.StakeKeys.hprop_golden_shelleyStakeKeys_bech32

  t153 <-
    pure $
      H.testPropertyNamed
        "golden shelleyVRFKeys"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys.hprop_golden_shelleyVRFKeys")
        Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys.hprop_golden_shelleyVRFKeys

  t154 <-
    pure $
      H.testPropertyNamed
        "golden shelleyVRFKeys te"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys.hprop_golden_shelleyVRFKeys_te")
        Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys.hprop_golden_shelleyVRFKeys_te

  t155 <-
    pure $
      H.testPropertyNamed
        "golden shelleyVRFKeys bech32"
        (fromString "Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys.hprop_golden_shelleyVRFKeys_bech32")
        Test.Golden.Shelley.TextEnvelope.Keys.VRFKeys.hprop_golden_shelleyVRFKeys_bech32

  t156 <-
    pure $
      H.testPropertyNamed
        "golden shelleyTx"
        (fromString "Test.Golden.Shelley.TextEnvelope.Tx.Tx.hprop_golden_shelleyTx")
        Test.Golden.Shelley.TextEnvelope.Tx.Tx.hprop_golden_shelleyTx

  t157 <-
    pure $
      H.testPropertyNamed
        "golden shelleyWitness"
        (fromString "Test.Golden.Shelley.TextEnvelope.Tx.Witness.hprop_golden_shelleyWitness")
        Test.Golden.Shelley.TextEnvelope.Tx.Witness.hprop_golden_shelleyWitness

  t158 <-
    pure $
      H.testPropertyNamed
        "golden shelleyTextViewDecodeCbor"
        (fromString "Test.Golden.Shelley.TextView.DecodeCbor.hprop_golden_shelleyTextViewDecodeCbor")
        Test.Golden.Shelley.TextView.DecodeCbor.hprop_golden_shelleyTextViewDecodeCbor

  t159 <-
    pure $
      H.testPropertyNamed
        "golden shelleyTransactionAssembleWitness SigningKey"
        ( fromString
            "Test.Golden.Shelley.Transaction.Assemble.hprop_golden_shelleyTransactionAssembleWitness_SigningKey"
        )
        Test.Golden.Shelley.Transaction.Assemble.hprop_golden_shelleyTransactionAssembleWitness_SigningKey

  t160 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction build"
        (fromString "Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build")
        Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build

  t161 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction build minting"
        (fromString "Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_minting")
        Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_minting

  t162 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction build withdrawal script witnessed"
        ( fromString
            "Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_withdrawal_script_witnessed"
        )
        Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_withdrawal_script_witnessed

  t163 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction build txin script witnessed"
        ( fromString
            "Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_txin_script_witnessed"
        )
        Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_txin_script_witnessed

  t164 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction build txout inline datum"
        ( fromString
            "Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_txout_inline_datum"
        )
        Test.Golden.Shelley.Transaction.Build.hprop_golden_shelley_transaction_build_txout_inline_datum

  t165 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction id"
        (fromString "Test.Golden.Shelley.Transaction.Id.hprop_golden_shelley_transaction_id")
        Test.Golden.Shelley.Transaction.Id.hprop_golden_shelley_transaction_id

  t166 <-
    pure $
      H.testPropertyNamed
        "golden shelley transaction sign"
        (fromString "Test.Golden.Shelley.Transaction.Sign.hprop_golden_shelley_transaction_sign")
        Test.Golden.Shelley.Transaction.Sign.hprop_golden_shelley_transaction_sign

  t167 <-
    pure $
      H.testPropertyNamed
        "golden view babbage yaml"
        (fromString "Test.Golden.TxView.hprop_golden_view_babbage_yaml")
        Test.Golden.TxView.hprop_golden_view_babbage_yaml

  t168 <-
    pure $
      H.testPropertyNamed
        "golden view metadata"
        (fromString "Test.Golden.TxView.hprop_golden_view_metadata")
        Test.Golden.TxView.hprop_golden_view_metadata

  t169 <-
    pure $
      H.testPropertyNamed
        "golden view conway proposal"
        (fromString "Test.Golden.TxView.hprop_golden_view_conway_proposal")
        Test.Golden.TxView.hprop_golden_view_conway_proposal

  t170 <-
    pure $
      H.testPropertyNamed
        "golden version"
        (fromString "Test.Golden.Version.hprop_golden_version")
        Test.Golden.Version.hprop_golden_version

  pure $
    T.testGroup
      "test/cardano-cli-golden/cardano-cli-golden.hs"
      [ t0
      , t1
      , t2
      , t3
      , t4
      , t5
      , t6
      , t7
      , t8
      , t9
      , t10
      , t11
      , t12
      , t13
      , t14
      , t15
      , t16
      , t17
      , t18
      , t19
      , t20
      , t21
      , t22
      , t23
      , t29
      , t30
      , t31
      , t32
      , t33
      , t34
      , t35
      , t36
      , t37
      , t38
      , t39
      , t40
      , t41
      , t42
      , t43
      , t44
      , t45
      , t46
      , t47
      , t48
      , t49
      , t50
      , t51
      , t52
      , t53
      , t54
      , t55
      , t56
      , t57
      , t58
      , t59
      , t60
      , t61
      , t62
      , t63
      , t64
      , t65
      , t66
      , t67
      , t68
      , t69
      , t70
      , t71
      , t72
      , t73
      , t74
      , t75
      , t76
      , t77
      , t78
      , t79
      , t80
      , t81
      , t82
      , t83
      , t84
      , t85
      , t86
      , t87
      , t88
      , t89
      , t91
      , t92
      , t93
      , t94
      , t95
      , t96
      , t97
      , t98
      , t99
      , t100
      , t101
      , t102
      , t103
      , t104
      , t105
      , t106
      , t107
      , t108
      , t109
      , t110
      , t111
      , t112
      , t113
      , t114
      , t115
      , t116
      , t117
      , t118
      , t119
      , t120
      , t121
      , t122
      , t123
      , t124
      , t125
      , t126
      , t127
      , t128
      , t129
      , t130
      , t131
      , t132
      , t133
      , t134
      , t135
      , t136
      , t137
      , t138
      , t139
      , t140
      , t141
      , t142
      , t143
      , t144
      , t145
      , t146
      , t147
      , t148
      , t149
      , t150
      , t151
      , t152
      , t153
      , t154
      , t155
      , t156
      , t157
      , t158
      , t159
      , t160
      , t161
      , t162
      , t163
      , t164
      , t165
      , t166
      , t167
      , t168
      , t169
      , t170
      ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs
  E.withArgs ([] ++ args) $ tests >>= T.defaultMainWithIngredients ingredients
