{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Committee where

import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Key


data AnyCommitteeCertificateTarget where
  HotKeyAuthCert
    :: ConwayEraOnwards era
    -> VerificationKeyOrHashOrFile CommitteeColdKey
    -> VerificationKeyOrHashOrFile CommitteeHotKey
    -> AnyCommitteeCertificateTarget
  ColdKeyResignationCert
    :: ConwayEraOnwards era
    -> VerificationKeyOrHashOrFile CommitteeColdKey
    -> AnyCommitteeCertificateTarget
