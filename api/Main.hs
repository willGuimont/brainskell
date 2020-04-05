{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Lens
import Data.List.Utils (replace)
import Data.Strings
import System.IO.Capture

import Control.Monad.Cont (liftIO)
import Data.Maybe (fromMaybe)
import Run
import System.Environment

newtype BrainskellInput =
  BrainskellInput
    { program :: T.Text
    }
  deriving (Generic, Show)

makeLenses ''BrainskellInput

instance FromJSON BrainskellInput

data BrainskellOutput
  = BrainskellOutput
      { output :: T.Text
      }
  | CompilationError
  deriving (Generic, Show)

makeLenses ''BrainskellOutput

instance ToJSON BrainskellOutput

-- API specification
type BrainskellApi = "run" :> ReqBody '[ JSON] BrainskellInput :> Post '[ JSON] BrainskellOutput

brainskellApi :: Proxy BrainskellApi
brainskellApi = Proxy

-- server-side handler (one per endpoint)
server :: Server BrainskellApi
server = runBrainskell
  where
    runBrainskell pg = do
      let p = replace "," "" $ T.unpack $ program pg
      case readBrainfuck p of
        Right bf -> do
          (out, _, _, _) <- liftIO (capture (evalBrainfuck emptyTape bf))
          let str = toString out
          return . BrainskellOutput $ T.pack str
        Left _ -> return CompilationError

-- server to WAI app
wai :: Application
wai = serve brainskellApi server

runBrainskellServer :: Port -> IO ()
runBrainskellServer port = do
  let settings = setTimeout 5 $ setPort port defaultSettings
  runSettings settings wai

main :: IO ()
main = do
  portEither <- lookupEnv "PORT"
  let port = fromMaybe "8001" portEither
  runBrainskellServer $ read port