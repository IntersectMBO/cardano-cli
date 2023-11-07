
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use let" -}

module Cardano.CLI.EraBased.Run.Governance.Hash
  ( runGovernanceHashCmds
  ) where

import           Cardano.Api

import qualified Cardano.CLI.EraBased.Commands.Governance.Hash as Cmd
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceCmdError
import           Cardano.CLI.Types.Errors.GovernanceHashError
import           Cardano.Crypto.Hash (hashToTextAsHex)
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Crypto
import           Cardano.Ledger.SafeHash (extractHash)
import qualified Cardano.Ledger.SafeHash as Ledger

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.ByteString as BS
import           Data.Function
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

runGovernanceHashCmds :: ()
  => Cmd.GovernanceHashCmds era
  -> ExceptT CmdError IO ()
runGovernanceHashCmds (Cmd.GovernanceHashCmd args)=
    runGovernanceHashCmd args
      & firstExceptT (CmdGovernanceCmdError . GovernanceCmdHashError)

runGovernanceHashCmd :: ()
  => Cmd.GovernanceHashCmdArgs era
  -> ExceptT GovernanceHashError IO ()
runGovernanceHashCmd Cmd.GovernanceHashCmdArgs { toHash } =
  -- TODO @smelc we probably want an option to write the computed hash to a file
  -- This can be done in a separate PR
  case toHash of
    Cmd.GovernanceHashSourceBinaryFile fp -> do
      bytes <- liftIO $ BS.readFile $ unFile fp
      let hash = Ledger.hashAnchorData $ Ledger.AnchorData bytes
      printHash hash
    Cmd.GovernanceHashSourceTextFile fp -> do
      text <- liftIO $ Text.readFile $ unFile fp
      let hash = Ledger.hashAnchorData $ Ledger.AnchorData $ Text.encodeUtf8 text
      printHash hash
    Cmd.GovernanceHashSourceText text -> do
      let hash = Ledger.hashAnchorData $ Ledger.AnchorData $ Text.encodeUtf8 text
      printHash hash
  where
    printHash :: Ledger.SafeHash StandardCrypto i -> ExceptT GovernanceHashError IO ()
    printHash = liftIO . putStr . Text.unpack . hashToTextAsHex . extractHash
