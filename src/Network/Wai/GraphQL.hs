{-# language OverloadedLists     #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
module Network.Wai.GraphQL (
  graphQLApp
, noSubscriptionsGraphQLApp
, webSocketsGraphQLApp
, module Language.GraphQL.Type
) where

import           Control.Applicative            ((<|>))
import           Control.Monad                  (join)
import           Control.Monad.Catch
import           Data.Aeson                     ((.:), (.:?), (.=))
import qualified Data.Aeson                     as JSON
import           Data.Aeson.Text                (encodeToLazyText)
import           Data.ByteString.Lazy           (fromStrict, toStrict)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeUtf8')
import           Data.Text.Encoding.Error       (UnicodeException (..))
import qualified Data.Text.Lazy.Encoding        as T
import           Language.GraphQL
import           Language.GraphQL.Type
import           Network.HTTP.Types.Header      (hContentType)
import           Network.HTTP.Types.Method      (StdMethod (..), parseMethod)
import           Network.HTTP.Types.Status      (ok200)
import           Network.Wai
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import           Network.Wai.GraphQL.WebSockets

graphQLApp
  :: Schema IO
  -> Application
graphQLApp schema
  = WS.websocketsOr WS.defaultConnectionOptions
                    (webSocketsGraphQLApp schema)
                    (noSubscriptionsGraphQLApp schema)

noSubscriptionsGraphQLApp
  :: Schema IO
  -> Application
noSubscriptionsGraphQLApp schema req res =
  case parseMethod (requestMethod req) of
    Left err  -> toError $ either unpackUnicodeException Prelude.id (decodeUtf8' err)
    Right GET -> do
      let qst = queryString req
          opN = decodeUtf8' <$> join (lookup "operationName" qst)
          decodedQuery = fmap decodeUtf8' =<< lookup "query" qst
      case (decodedQuery, lookup "variables" qst) of
        (Just (Right qry), Just (Just vars)) ->
          case JSON.eitherDecode $ fromStrict vars of
            Left err  -> toError $ T.pack err
            Right vrs -> case sequence opN of
              Left err     -> toError $
                "Could not parse operation name: " <> unpackUnicodeException err
              Right opName -> execQuery opName vrs qry
        (Just (Right qry), _) -> case sequence opN of
              Left err     -> toError $
                "Could not parse query: " <> unpackUnicodeException err
              Right opName -> execQuery opName HM.empty qry
        _ -> toError "Error parsing query"
    Right POST -> do
      body <- strictRequestBody req
      case lookup hContentType $ requestHeaders req of
        Just "application/json"    ->
          case JSON.eitherDecode body of
            Left err                             -> toError $ T.pack err
            Right (GraphQLInput qry vars opName) -> execQuery opName vars qry
        Just "application/graphql" ->
          case decodeUtf8' $ toStrict body of
            Left err  -> toError $
              "Could not decode utf8 from body: " <> unpackUnicodeException err
            Right msg -> execQuery Nothing HM.empty msg
        _ -> toError "No `Content-Type` header found!"
    _ -> toError "Unsupported method"
  where
    execQuery :: Maybe T.Text -> JSON.Object -> T.Text -> IO ResponseReceived
    execQuery opn vals qry = (do
      result <- graphqlSubs schema opn vals qry
      case result of
        Left _str -> toError "Streaming is not supported"
        Right doc -> toResponse (JSON.Object doc))
      `catch`
      (\(err :: SomeException) -> toError $ T.pack $ show err)
    toError :: T.Text -> IO ResponseReceived
    toError err = toResponse $
      JSON.object [ "errors" .= JSON.Array [ JSON.object [ "message" .= JSON.String err ] ]]
    toResponse :: JSON.Value -> IO ResponseReceived
    toResponse = res . responseBuilder ok200 [] . T.encodeUtf8Builder . encodeToLazyText
    unpackUnicodeException :: UnicodeException -> T.Text
    unpackUnicodeException (DecodeError str _) = T.pack str

-- Intermediate data type to parse the input

data GraphQLInput = GraphQLInput T.Text JSON.Object (Maybe T.Text)

instance JSON.FromJSON GraphQLInput where
  parseJSON = JSON.withObject "GraphQLInput" $
     \v -> GraphQLInput
      <$> v .: "query"
      <*> (v .: "variables" <|> pure HM.empty)
      <*> v .:? "operationName"
