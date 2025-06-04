{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.EraBased.Script.Certificate.Read
  (
  )
where

import Cardano.Api

import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Type.Common (CertificateFile)

import Control.Monad
