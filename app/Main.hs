{-# LANGUAGE DuplicateRecordFields, ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
module Main where

import ClassyPrelude
import GitAnnexSpecialRemote
import Control.Monad.Free
import Data.Either.Combinators (maybeToRight)
import Control.Monad.Trans.Except
import qualified Data.List as L
import qualified System.Environment as S
import qualified Data.Text.IO as T
import System.Exit
import System.IO (isEOF)
import Control.Monad.Trans.Maybe
import qualified Network.Wreq.Session as Sess
import Control.Monad.Trans.State
import qualified B2


newtype BucketPrefix = BucketPrefix { unBucketPrefix :: Text } deriving (Eq, Show)

data AppConfig =
  AppConfig  { accountId :: B2.AccountId
          , appKey :: B2.AppKey
          , bucket :: B2.Bucket
          , prefix :: Maybe BucketPrefix
          } deriving (Show)

data AppCtx =
  AppCtx { session :: Sess.Session }

data AppState =
  AppState { config :: Maybe AppConfig
           , authorizationToken :: Maybe Text
           , recommendedPartSize :: Maybe Int
           }

runApp :: Free AnnexSpecialRemote a -> AppCtx -> IO a
runApp app ctx = evalStateT (eval app) (AppState Nothing Nothing Nothing)
  where
    eval :: Free AnnexSpecialRemote a -> StateT AppState IO a
    eval cmd = do
      appState <- get
      case cmd of
        (Free (CheckPresent key f)) -> eval (f True)
        (Free (Exit (Fatal msg))) -> liftIO $ T.hPutStrLn stderr msg >> exitFailure
        (Free (Exit Success)) -> liftIO exitSuccess
        (Free (Init annexConf f)) ->
          (liftIO . runExceptT $ do
            appConfig <- ExceptT $ getAppConfig annexConf
            b2Resp <- ExceptT $ B2.auth (accountId appConfig) (appKey appConfig) (session ctx)
            return $ appState { authorizationToken = Just $ B2.authorizationToken b2Resp
                              , recommendedPartSize = Just $ B2.recommendedPartSize b2Resp
                              })
          >>= either (eval . f . Just) (\newState -> put newState >> eval (f Nothing))
        (Free (Prepare f)) -> eval (f Nothing)
        (Free (IsStdInEOF f)) -> (liftIO $ isEOF) >>= eval . f
        (Free (ReadLine f)) -> (liftIO $ getLine) >>= eval . f . parseAnnexRequestMessage
        (Free (Reply reply f)) -> (liftIO $ putStrLn (asAnnexProtoMsg reply)) >> eval f
        (Free (SendCmd cmd f)) -> (liftIO $ putStrLn (asAnnexProtoMsg cmd)) >> eval f
        (Free (Remove _ f)) -> eval (f Nothing)
        (Free (Retrieve _ _ f)) -> eval (f Nothing)
        (Free (Store _ _ f)) -> eval (f Nothing)
        (Pure a) -> return a

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- Important or the app doesn't work and annex hangs waiting for messages
  sess <- Sess.newAPISession
  runApp (annexSpecialRemoteApp ["bucket", "prefix"]) (AppCtx sess)

getAppConfig :: [(Text, Text)] -> IO (Either Text AppConfig)
getAppConfig gitAnnexConfig = runExceptT $ do
  accountId' <- B2.AccountId <$> required "B2_ACCOUNT_ID"
  appKey' <- B2.AppKey <$> required "B2_APP_KEY"
  bucket' <- maybe (throwE "ERROR - No bucket set, you must specify the B2 bucket to use i.e bucket=mybucket") (return . B2.Bucket) (getAnnexConfValue "bucket")
  let prefix' = BucketPrefix <$> getAnnexConfValue "prefix"
  return $ AppConfig accountId' appKey' bucket' prefix'
  where
    env :: String -> IO (Maybe Text)
    env =  runMaybeT . fmap pack . MaybeT . S.lookupEnv

    getAnnexConfValue :: Text -> Maybe Text
    getAnnexConfValue a = fmap snd . L.find((==) a . fst) $ gitAnnexConfig

    required :: String -> ExceptT Text IO Text
    required key = ExceptT (maybeToRight ("ERROR - " <> pack key <> " required environment variable is not set") <$> env key)
