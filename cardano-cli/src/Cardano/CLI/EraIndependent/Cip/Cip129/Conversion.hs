{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Cip.Cip129.Conversion
  ( encodeCip129DrepVerficationKeyText
  , encodeCip129CommitteeColdVerficationKeyText
  , encodeCip129CommitteeHotVerficationKeyText
  , encodeCip129GovernanceActionIdText
  )
where

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.Read.Committee.ColdKey
import Cardano.CLI.Read.Committee.HotKey
import Cardano.CLI.Read.DRep

import Data.Text

encodeCip129DrepVerficationKeyText :: AnyDrepVerificationKey -> Text
encodeCip129DrepVerficationKeyText = serialiseToBech32Cip129 . anyDrepVerificationKeyToCredential

anyDrepVerificationKeyToCredential
  :: AnyDrepVerificationKey -> L.Credential L.DRepRole
anyDrepVerificationKeyToCredential drepKey =
  case drepKey of
    AnyDrepVerificationKey vk ->
      let DRepKeyHash hash = verificationKeyHash vk
       in L.KeyHashObj hash
    AnyDrepExtendedVerificationKey vk ->
      let DRepExtendedKeyHash hash = verificationKeyHash vk
       in L.KeyHashObj hash

encodeCip129CommitteeHotVerficationKeyText :: AnyCommitteeHotVerificationKey -> Text
encodeCip129CommitteeHotVerficationKeyText = serialiseToBech32Cip129 . anyCommitteeHotVerificationKeyToCredential

anyCommitteeHotVerificationKeyToCredential
  :: AnyCommitteeHotVerificationKey -> L.Credential L.HotCommitteeRole
anyCommitteeHotVerificationKeyToCredential committeeHotKey =
  case committeeHotKey of
    AnyCommitteeHotVerificationKey vk ->
      let CommitteeHotKeyHash hash = verificationKeyHash vk
       in L.KeyHashObj hash
    AnyCommitteeHotExtendedVerificationKey vk ->
      let CommitteeHotExtendedKeyHash hash = verificationKeyHash vk
       in L.KeyHashObj hash

encodeCip129CommitteeColdVerficationKeyText :: AnyCommitteeColdVerificationKey -> Text
encodeCip129CommitteeColdVerficationKeyText = serialiseToBech32Cip129 . anyCommitteeColdVerificationKeyToCredential

anyCommitteeColdVerificationKeyToCredential
  :: AnyCommitteeColdVerificationKey -> L.Credential L.ColdCommitteeRole
anyCommitteeColdVerificationKeyToCredential committeeColdKey =
  case committeeColdKey of
    AnyCommitteeColdVerificationKey vk ->
      let CommitteeColdKeyHash hash = verificationKeyHash vk
       in L.KeyHashObj hash
    AnyCommitteeColdExtendedVerificationKey vk ->
      let CommitteeColdExtendedKeyHash hash = verificationKeyHash vk
       in L.KeyHashObj hash

encodeCip129GovernanceActionIdText :: L.GovActionId -> Text
encodeCip129GovernanceActionIdText = serialiseGovActionIdToBech32Cip129
