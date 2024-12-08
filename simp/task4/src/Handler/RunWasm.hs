{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Handler.RunWasm where

import Import
import Data.Aeson
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

data RunWasmRequest = RunWasmRequest
    { wasmPath :: Text
    , wasmInput :: Text
    }

instance FromJSON RunWasmRequest where
    parseJSON = withObject "RunWasmRequest" $ \v -> RunWasmRequest
        <$> v .: "wasmPath"
        <*> v .: "wasmInput"

postRunWasmR :: Handler Value
postRunWasmR = do
    RunWasmRequest { wasmPath, wasmInput } <- requireCheckJsonBody :: Handler RunWasmRequest

    let command = "wasmtime"
    let args = ["--invoke", "temp", unpack wasmPath, unpack wasmInput]

    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode command args ""

    case exitCode of
        ExitSuccess -> returnJson $ object ["output" .= stdout]
        ExitFailure _ -> returnJson $ object ["error" .= ("Execution failed: " <> stderr)]
