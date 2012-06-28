{-# LANGUAGE OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Github.Private where

import Github.Data
import Data.Aeson
import Data.Attoparsec.ByteString.Lazy
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Types (Method, Status(..))
import Network.HTTP.Conduit
import Data.Conduit (ResourceT)
import Text.URI
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)

data GithubConfig = GithubConfig { auth :: BasicAuth
                                 , endpoint :: String
                                 } deriving (Read, Show)

newtype Github a = Github { fromGithub :: ReaderT GithubConfig IO a }
                 deriving (Monad, MonadIO, Functor)

class MonadGithub m where
  getEndpoint :: m String
  doRequest :: (FromJSON a, Show a) => Request IO -> m (Either Error a)

instance MonadGithub IO where
  getEndpoint = return "https://api.github.com/"
  doRequest request = do
    result <- (getResponse request >>= return . Right) `E.catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      E.Handler (\e -> E.throw (e :: E.AsyncException)),
      E.Handler (\e -> (return . Left) (e :: E.SomeException))
      ]
    return $ either (Left . HTTPConnectionError)
      (parseJson . responseBody)
      result    
  where
    getResponse request = withManager $ \manager -> httpLbs request manager
  
instance MonadGithub Github where
  getEndpoint = Github $ fmap endpoint ask
  doRequest = authRequest >=> (liftIO . doRequest)

get, post, patch, put, delete :: ByteString
get = BS.pack "GET"
post = BS.pack "POST"
patch = BS.pack "PATCH"
put = BS.pack "PUT"
delete = BS.pack "DELETE"

authRequest :: Request IO -> Github (Request IO)
authRequest req = do
 info <- auth <$> ask
 return $ (uncurry applyBasicAuth info) req

githubGet :: (MonadGithub m, FromJSON a, Show a) => [String] -> m (Either Error a)
githubGet = buildUrl >=> githubAPI get Nothing

githubGetWithQueryString :: (MonadGithub m, FromJSON a, Show a) => [String] -> String -> m (Either Error a)
githubGetWithQueryString = githubAPI get
                           (buildUrl paths ++ "?" ++ queryString)
                           (Nothing :: Maybe Value)

githubPost :: (FromJSON a, Show a) => [String] -> a -> Github (Either Error b)
githubPost paths body = buildUrl paths >>= \url -> githubAPI post (Just body)

githubPost :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> Github (Either Error b)
githubPost auth paths body =
  githubAPI post
            (buildUrl paths)
            (Just body)

githubPatch :: (ToJSON a, Show a, FromJSON b, Show b) => [String] -> a -> Github (Either Error b)
githubPatch auth paths body =
  githubAPI patch
            (buildUrl paths)
            (Just body)

buildUrl :: (Functor m, MonadGithub m) => [String] -> m String
buildUrl paths = (intercalate "/" . (:paths)) <$> getEndpoint

githubAPI :: (ToJSON a, Show a, FromJSON b, Show b) => BS.ByteString -> Maybe a -> String -> IO (Either Error b)
githubAPI method body url = do
  result <- doHttps method url (Just encodedBody)
  return $ either (Left . HTTPConnectionError)
                  (parseJson . responseBody)
                  result
  where encodedBody = RequestBodyLBS $ encode $ toJSON body

-- | user/password for HTTP basic access authentication
type BasicAuth = (BS.ByteString, BS.ByteString)

makeRequest :: Method -> String -> Maybe (RequestBody (ResourceT IO)) -> Request IO
makeRequest method url body = uri { method = method
                                  , secure = True
                                  , port = 443
                                  , requestBody = requestBody
                                  , checkStatus = successOrMissing
                                  }
  where
    requestBody = fromMaybe (RequestBodyBS BS.empty) body
    (Just uri) = parseUrl url
    successOrMissing s@(Status sci _) hs
      | (200 <= sci && sci < 300) || sci == 404 = Nothing
      | otherwise = Just $ E.toException $ StatusCodeException s hs

parseJson :: (FromJSON b, Show b) => LBS.ByteString -> Either Error b
parseJson jsonString =
  let parsed = parse (fromJSON <$> json) jsonString in
  case parsed of
       Data.Attoparsec.ByteString.Lazy.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> Right s
              (Error e) -> Left $ JsonError $ e ++ " on the JSON: " ++ LBS.unpack jsonString
       (Fail _ _ e) -> Left $ ParseError e
