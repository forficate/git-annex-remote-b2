{-# LANGUAGE DeriveFunctor, FlexibleContexts, LambdaCase, TemplateHaskell #-}
module GitAnnexSpecialRemote (
         AnnexSpecialRemote(..),
         ExitStatus(..),
         Key(..),
         annexSpecialRemoteApp,
         asAnnexProtoMsg,
         parseAnnexRequestMessage
       ) where

import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid((<>))
import System.FilePath()
import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import Data.Bool (bool)
import Data.Maybe (maybeToList)
import Control.Monad.Trans.Maybe

newtype Key = Key { unKey :: Text } deriving (Eq, Show)
newtype Password = Password { unPassword :: Text } deriving (Eq, Show)
newtype Remote = Remote { unRemote :: Text } deriving (Eq, Show)
newtype Username = Username { unUsername :: Text } deriving (Eq, Show)
type Error = Text

-- | The 'AnnexRequestMessage' type represents values sent from GitAnnex
data AnnexRequestMessage
  = AnnexErrorReq Error
  | CheckpresentReq Key
  | ConfValue (Maybe Text)
  | InitremoteReq
  | PrepareReq
  | RemoveReq Key
  | RetrieveReq Key FilePath
  | StoreReq Key FilePath
  | UnkownReq Text
  deriving (Show)


-- | The 'AnnexProtoMsg' is used to create raw protocol/wire messages to be sent to GitAnnex via stdout
class AnnexProtoMsg  a where
  asAnnexProtoMsg :: a -> Text

-- | The 'AnnexCmd' type represents commands sent to GitAnnex
data AnnexCmd
  = GetCreds Remote
  | GetConfig Text
  | Progress Int
  | SendError Text
  | SetCreds Remote Username Password
  | Version Int


instance AnnexProtoMsg AnnexCmd where
  asAnnexProtoMsg (GetCreds remote) = "GETCREDS " <> unRemote remote
  asAnnexProtoMsg (GetConfig key)   = "GETCONFIG " <> key
  asAnnexProtoMsg (Progress n)      = "PROGRESS " <> T.pack (show n)
  asAnnexProtoMsg (SendError msg)   = "ERROR " <> msg
  asAnnexProtoMsg (SetCreds r u p)  = "SETCREDS " <> unRemote r <> " " <> unUsername u <> " " <> unPassword p
  asAnnexProtoMsg (Version n)       = "VERSION " <> T.pack (show n)

instance AnnexProtoMsg AnnexReply where
  asAnnexProtoMsg (CheckPresentFailure key) = "CHECKPRESENT-FAILURE " <> unKey key
  asAnnexProtoMsg (CheckPresentSuccess key) = "CHECKPRESENT-SUCCESS " <> unKey key
  asAnnexProtoMsg (InitremoteFailure msg)   = "INITREMOTE-FAILURE " <> msg
  asAnnexProtoMsg (PrepareFailure msg)      = "PREPARE-FAILRE " <> msg
  asAnnexProtoMsg (RemoveFailure key file)  = "REMOVE-FAILURE " <> unKey key <> " " <> file
  asAnnexProtoMsg (RemoveSuccess key)       = "REMOVE-SUCCESS " <> unKey key
  asAnnexProtoMsg (RetrieveFailure key err) = "TRANSFER-FAILURE RETRIEVE " <> unKey key <> " " <> err
  asAnnexProtoMsg (RetrieveSuccess key)     = "TRANSFER-SUCCESS RETRIEVE " <> unKey key
  asAnnexProtoMsg (StoreSuccess key)        = "TRANSFER-SUCCESS STORE " <> unKey key
  asAnnexProtoMsg (StoreFailure key err)    = "TRANSFER-FAILURE STORE " <> unKey key <> " " <> err
  asAnnexProtoMsg InitremoteSuccess         = "INITREMOTE-SUCCESS"
  asAnnexProtoMsg PrepareSuccess            = "PREPARE-SUCCESS"
  asAnnexProtoMsg Unsupported               = "UNSUPPORTED-REQUEST"



-- | The 'AnnexReply' type represents responses returned to GitAnnex in response to AnnexRequestMessage's
data AnnexReply
  = CheckPresentFailure Key
  | CheckPresentSuccess Key
  | InitremoteFailure Text
  | InitremoteSuccess
  | PrepareFailure Text
  | PrepareSuccess
  | RemoveFailure Key Text
  | RemoveSuccess Key
  | RetrieveFailure Key Text
  | RetrieveSuccess Key
  | StoreFailure Key Text
  | StoreSuccess Key
  | Unsupported
  deriving (Show)

data ExitStatus = Success | Fatal Text

data AnnexSpecialRemote next
  = CheckPresent Key (Bool -> next)
  | Exit ExitStatus
  | Init [(Text, Text)] (Maybe Error -> next)
  | IsStdInEOF (Bool -> next)
  | Prepare (Maybe Error -> next)
  | ReadLine (AnnexRequestMessage -> next)
  | Remove Key (Maybe Error -> next)
  | Reply AnnexReply next
  | Retrieve Key String (Maybe Error -> next)
  | SendCmd AnnexCmd next
  | Store Key String (Maybe Error -> next)
  deriving (Functor)

type AnnexAction a = Free AnnexSpecialRemote a

makeFree ''AnnexSpecialRemote

{-- | The core app logic
  Loops around trying to read requests from git-annex sending "VERSION 1" until annex closes the connection with EOF
  The attempt is to be as generic as possible and not leak B2 info so this can be reused for other remotes
  Use of free allows for easily expressing the logic -- I can test the logic when i get time to play with hspec/quick check
--}
annexSpecialRemoteApp :: [Text] -> AnnexAction ()
annexSpecialRemoteApp confKeys = sendCmd (Version 1) >> loop
 where
  loop = isStdInEOF >>= \eof -> if eof
    then exit Success
    else
       readLine >>= \case
        AnnexErrorReq msg -> exit $ Fatal msg
        CheckpresentReq key ->
          checkPresent key
            >>= reply . bool (CheckPresentFailure key) (CheckPresentSuccess key)
            >>  loop
        ConfValue _ -> Pure ()
        InitremoteReq ->
          getConfigValues confKeys
            >>= GitAnnexSpecialRemote.init
            >>= reply . maybe InitremoteSuccess InitremoteFailure
            >>  loop
        PrepareReq ->
          prepare >>= reply . maybe PrepareSuccess PrepareFailure >> loop
        RemoveReq key ->
          remove key
            >>= reply . maybe (RemoveSuccess key) (RemoveFailure key)
            >>  loop
        RetrieveReq key file ->
          retrieve key file
            >>= reply . maybe (RetrieveSuccess key) (RetrieveFailure key)
            >>  loop
        StoreReq key file ->
          store key file
            >>= reply . maybe (StoreSuccess key) (StoreFailure key)
            >>  loop
        UnkownReq msg ->
          reply Unsupported
            >> loop



getConfigValues :: [Text] -> AnnexAction [(Text, Text)]
getConfigValues = foldM (\result key -> (<>) result <$> f key) []
  where
    f key =  maybeToList <$> runMaybeT ((,) key <$> (MaybeT $ getConfig key))


getConfig :: Text -> AnnexAction (Maybe Text)
getConfig key = do
  _    <- sendCmd $ GetConfig key
  resp <- readLine
  case resp of
    ConfValue v -> return v
    _           -> return Nothing -- TODO, currently swallows unexpected responses, is this ok?...no but it works


parseAnnexRequestMessage :: Text -> AnnexRequestMessage
parseAnnexRequestMessage txt = either (\_ -> UnkownReq txt)
                                      id
                                      (Parsec.parse parser "" txt)
 where
  arg0 cmd = Parsec.try (Parsec.string cmd)

  arg1 cmd = do
    _     <- Parsec.try (Parsec.string cmd)
    _     <- Parsec.space
    value <- Parsec.manyTill Parsec.anyChar Parsec.eof
    return $ T.pack value

  arg2 cmd = do
    _ <- Parsec.try $ Parsec.string cmd
    _ <- Parsec.space
    a <- Parsec.manyTill Parsec.anyChar Parsec.space
    b <- Parsec.manyTill Parsec.anyChar Parsec.eof
    return (T.pack a, b)

  asMaybe s = if T.null s then Nothing else Just s

  parser =
    (AnnexErrorReq <$> arg1 "ERROR")
      <|> (CheckpresentReq . Key <$> arg1 "CHECKPRESENT")
      <|> (ConfValue . asMaybe <$> arg1 "VALUE")
      <|> (InitremoteReq <$ arg0 "INITREMOTE")
      <|> (PrepareReq <$ arg0 "PREPARE")
      <|> (RemoveReq . Key <$> arg1 "REMOVE")
      <|> ((\(key, file) -> RetrieveReq (Key key) file) <$> arg2 "RETRIEVE")
      <|> ((\(key, file) -> StoreReq (Key key) file) <$> arg2 "STORE")

