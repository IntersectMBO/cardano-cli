{-# LANGUAGE DataKinds #-}

module Cardano.CLI.EraIndependent.Debug.CertificateView.Command where

import Cardano.CLI.Type.Common

import Vary (Vary)

data CertificateViewCmdArgs = CertificateViewCmdArgs
  { outputFormat :: !(Vary [FormatJson, FormatYaml])
  , mOutFile :: !(Maybe (File () Out))
  , inputCertificateFile :: !CertificateFile
  }
  deriving Show
