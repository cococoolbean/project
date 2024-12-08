{-# LANGUAGE OverloadedStrings #-}
module Handler.Compile where

import Import
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.IO.Temp as Temp
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (getCurrentDirectory)
import Data.Time (getCurrentTime)

newtype Compile = Compile Text

instance ToJSON Compile where
    toJSON (Compile t) = object ["message" .= t]

instance FromJSON Compile where
    parseJSON = withObject "Compile" $ \o -> Compile <$> o .: "message"

postCompileR :: Handler Value
postCompileR = do
    currentTime <- liftIO getCurrentTime
    compile@(Compile text) <- requireCheckJsonBody :: Handler Compile

    let wasmFile = "/tmp/temp.wasm" 
        outputFile = "/tmp/temp"    
        simpFile = "/tmp/simp.simp"

    -- Write the SIMP code to a file
    liftIO $ TIO.writeFile simpFile text
    liftIO $ TIO.putStrLn $ "SIMP code written to: " <> T.pack simpFile
    currentDir <- liftIO getCurrentDirectory
    let command = currentDir </> ".stack-work/install/x86_64-linux/491227709979cde0824fabc19671f319aa9cd80bfe86e141d5356773d292d7fd/9.2.8/bin/simp"
    let args = ["-c", simpFile, wasmFile, "temp"]

    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode command args ""
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime

    let successMessage = "SIMP code compiled successfully at " <> formattedTime <> "!"
    case exitCode of
        ExitSuccess -> do
            liftIO $ TIO.putStrLn $ "Command succeeded. WASM file created at: " <> T.pack wasmFile
            liftIO $ TIO.putStrLn $ "Output file created at: " <> T.pack outputFile
        ExitFailure code -> do
            liftIO $ TIO.putStrLn $ "Command failed with code " <> T.pack (show code)
            liftIO $ TIO.putStrLn $ "Error: " <> T.pack stderr

    returnJson $ object
        [  "message" .= successMessage
        , "wasmFile" .= T.pack wasmFile
        , "outputFile" .= T.pack outputFile
        ]
