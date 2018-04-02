{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, NoImplicitPrelude, GeneralizedNewtypeDeriving #-}
module B2
  ( AccountId(..)
  , AppKey(..)
  , BucketName(..)
  , AuthResponse(..)
  , B2.auth
  , B2.bucketExists
  )where

import ClassyPrelude
import Control.Lens
import Control.Monad.Trans.Except
import Data.Aeson as A hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Lazy as LB (ByteString)
import Data.Either.Combinators (maybeToRight)
import Network.Wreq as Wreq
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Session


newtype AccountId = AccountId { unAccountId :: Text } deriving (Eq, Show, ToJSON)
newtype ApiBaseUrl = ApiBaseUrl { unApiBaseUrl :: String} deriving (Eq, Show)
newtype AppKey = AppKey { unAppKey :: Text } deriving (Eq, Show)
newtype AuthorizationToken = AuthorizationToken { unAuthorizationToken :: Text} deriving (Eq, Show)
newtype BucketId = BucketId { unBucketId :: Text } deriving (Eq, Show)
newtype BucketName = BucketName { unBucketName :: Text } deriving (Eq, Show)


type B2Action a = ReaderT B2Ctx IO a

data B2Ctx =
  B2Ctx { accountId :: AccountId
        , apiBaseUrl :: ApiBaseUrl
        , authToken :: AuthorizationToken
        , session :: Session
        }

data Bucket =
  Bucket { bucketId :: BucketId
         , bucketName :: BucketName
         }


data AuthResponse =
  AuthResponse { apiUrl :: ApiBaseUrl
               , authorizationToken :: AuthorizationToken
               , recommendedPartSize :: Int
               } deriving (Show)


defaultOpts :: Wreq.Options
defaultOpts = Wreq.defaults
  & (checkResponse ?~ (\ _ _ -> return ())) -- disable non 200 throwing exceptions
  & header "User-Agent" .~ ["git-annex-b2-remote-haskell"]


(\/>) :: Maybe a -> b -> Either b a
(\/>) = flip maybeToRight


auth :: AccountId -> AppKey -> Session -> IO (Either Text AuthResponse)
auth accountId' accountKey sess = do
  let wreqOpts = defaultOpts & Wreq.auth ?~ basicAuth (encodeUtf8 . unAccountId $ accountId') (encodeUtf8 . unAppKey $ accountKey)
  resp <- Session.getWith wreqOpts sess "https://api.backblazeb2.com/b2api/v1/b2_authorize_account"
  case resp ^. responseStatus . statusCode of
    200 -> return $ do
      let body = decodeUtf8 (toStrict(resp ^. responseBody))
      json <- (body ^? _Object) \/> ("Failed to parse JSON from B2 response: " <> body)
      apiUrl' <- (json ^? at "apiUrl" . _Just . _String) \/> ("Missing apiUrl value in response: " <> body)
      token <- (json ^? at "authorizationToken" . _Just . _String) \/> ("Missing authorizationToken value in response: " <> body)
      partSize <- (json ^? at "recommendedPartSize" . _Just . _Integral) \/> ("Missing or invalid recommendedPartSize value response: " <> body)
      return $ AuthResponse (ApiBaseUrl $ unpack apiUrl') (AuthorizationToken token) partSize
    code -> return $ Left $ "B2 returned status" <> pack (show code) <> ". " <>  decodeUtf8 (toStrict(resp ^. responseBody))


bucketExists :: BucketName -> B2Action (Either Text Bool)
bucketExists b = runExceptT $ any ((==) b . bucketName) <$> ExceptT listBuckets


listBuckets :: B2Action (Either Text [Bucket])
listBuckets = do
  accountId' <- asks accountId
  let reqBody = object [ "accountId" A..= accountId'
                       , "bucketTypes" A..=  [asText "allPrivate", asText "allPublic"]
                       ]
  resp <- b2Req $ Post "/b2api/v1/b2_list_buckets" reqBody
  case resp ^. responseStatus . statusCode of
    200 -> return $ do
      let body = decodeUtf8 (toStrict(resp ^. responseBody))
      json <- (body ^? _Object) \/> ("Failed to parse JSON from B2 response: " <> body)
      buckets <- (json ^? at "buckets" . _Just . _Array) \/> ("Missing or invalid buckets value response: " <> body)
      return $ map asBucket buckets ^.. traverse . _Just
    code -> return $ Left $ "B2 returned status" <> pack (show code) <> " for listBuckets. " <>  decodeUtf8 (toStrict(resp ^. responseBody))
  where
    asBucket value = Bucket <$> (BucketId <$> value ^? key "bucketId" . _String) <*> (BucketName <$> value ^? key "bucketName" . _String)

data B2Request = Post String Value

b2Req :: B2Request -> B2Action (Response LB.ByteString)
b2Req action = do
  authToken' <- encodeUtf8 . unAuthorizationToken <$> asks authToken
  session' <- asks session
  apiUrl' <- asks apiBaseUrl
  let wreqOpts = defaultOpts & header "Authorization" .~ [authToken']
  case action of
    Post url body -> liftIO $ Session.postWith wreqOpts session' (unApiBaseUrl apiUrl' <> url) body
