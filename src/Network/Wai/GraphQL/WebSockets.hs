{-# language FlexibleContexts    #-}
{-# language NamedFieldPuns      #-}
{-# language OverloadedLists     #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-
This module implements the protocol as specified in
https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
-}
module Network.Wai.GraphQL.WebSockets (
  webSocketsGraphQLApp
) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad            (forM_)
import           Control.Monad.Catch
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.Aeson               ((.:), (.:?), (.=))
import qualified Data.Aeson               as JSON
import           Data.Conduit
import           Data.Conduit.List        (sourceList)
import           Data.Foldable            (for_)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import           Language.GraphQL
import           Language.GraphQL.AST
import           Language.GraphQL.Error
import           Language.GraphQL.Type
import qualified ListT                    as L
import           Network.WebSockets       hiding (Response)
import qualified Network.WebSockets       as WS
import qualified StmContainers.Map        as M

webSocketsGraphQLApp :: Schema IO -> WS.ServerApp
webSocketsGraphQLApp schema conn
  = do let headers = WS.requestHeaders $ WS.pendingRequest conn
       case lookup "Sec-WebSocket-Protocol" headers of
         Just v
           | v == "graphql-ws" || v == "graphql-transport-ws"
           -> do conn' <- WS.acceptRequestWith conn (WS.AcceptRequest (Just v) [])
                 protocol schema conn'
         _ -> WS.rejectRequest conn "unsupported protocol"

protocol :: Schema IO
         -> Connection -> IO ()
protocol schema conn = start
  where
    -- listen for GQL_CONNECTION_INIT
    start = do
      msg <- receiveJSON conn
      case msg of
        Just (GQLConnectionInit _)
          -> do -- send GQL_CONNECTION_ACK
                sendJSON conn GQLConnectionAck
                vars <- M.newIO
                -- send GQL_KEEP_ALIVE each 1s.
                withAsync keepAlive $ \ka ->
                -- start listening for incoming messages
                  listen ka vars
        _ -> start  -- Keep waiting
    -- keep-alive
    keepAlive = do
      sendJSON conn GQLKeepAlive
      threadDelay 1000000
      keepAlive
    -- listen for messages from client
    listen ka vars = do
      msg <- receiveJSON conn
      case msg of
        Just (GQLStart i q v o)  -- start handling
          -> withAsync (handle i q v o >> atomically (M.delete i vars)) $ \t -> do
             atomically $ M.insert t i vars
             listen ka vars
        Just (GQLStop i)  -- stop with handling that query
          -> do r <- atomically $ M.lookup i vars
                case r of
                  Nothing -> return ()
                  Just a  -> do cancel a
                                atomically $ M.delete i vars
                listen ka vars
        Just GQLTerminate  -- terminate all queries
          -> do cancelAll ka vars
                sendClose conn ("GraphQL session terminated" :: T.Text)
        _ -> listen ka vars  -- Keep going
    -- Handle a single query
    handle i q v o = (do
      result <- graphqlSubs schema o v q
      let r = case result of
                Left  str -> str
                Right doc -> sourceList [Response (JSON.Object doc) []]
      runConduit $ r .| cndt i
      sendJSON conn (GQLComplete i))
      `catch`
      (\(err :: SomeException) -> sendJSON conn (GQLError i (JSON.toJSON (show err))))
    -- Conduit which sends the results via the wire
    cndt i = do
      msg <- await
      case msg of
        Nothing -> return ()
        Just (Response data' errors)  -> do
          liftIO $ sendJSON conn (GQLData i data')
          for_ errors $ \err -> liftIO $ sendJSON conn (GQLError i $ JSON.toJSON err)
          cndt i
    -- Cancel all pending subscriptions
    cancelAll ka vars
      = do cancel ka
           vs <- atomically $ L.toList $ M.listT vars
           forM_ (map snd vs) cancel

receiveJSON :: JSON.FromJSON a => Connection -> IO (Maybe a)
receiveJSON conn = do
  d <- receiveData conn
  return $ JSON.decode d

sendJSON :: JSON.ToJSON a => Connection -> a -> IO ()
sendJSON conn v
  = sendTextData conn (JSON.encode v)

data ClientMessage
  = GQLConnectionInit { initPayload :: Maybe JSON.Value }
  | GQLStart { clientMsgId   :: T.Text
             , query         :: T.Text
             , variables     :: JSON.Object
             , operationName :: Maybe T.Text}
  | GQLStop { clientMsgId :: T.Text }
  | GQLTerminate
  deriving Show

data ServerMessage
  = GQLConnectionError { errorPayload :: Maybe JSON.Value }
  | GQLConnectionAck
  | GQLData     { serverMsgId :: T.Text
                , payload     :: JSON.Value }
  | GQLError    { serverMsgId :: T.Text
                , payload     :: JSON.Value }
  | GQLComplete { serverMsgId :: T.Text}
  | GQLKeepAlive
  deriving Show

-- NOTE: using https://github.com/apollographql/subscriptions-transport-ws/blob/master/src/message-types.ts
-- as source of truth for the message types

instance JSON.FromJSON ClientMessage where
  parseJSON = JSON.withObject "ClientMessage" $ \v -> do
     ty :: String <- v .: "type"
     case ty of
       "connection_init"
         -> GQLConnectionInit <$> v .:? "payload"
       "start"
         -> do i <- v .: "id"
               (q,vrs,opN) <- v .: "payload" >>= parsePayload
               pure $ GQLStart i q vrs opN
       "stop"
         -> GQLStop <$> v .: "id"
       "terminate"
         -> pure GQLTerminate
       _ -> empty
    where
      parsePayload = JSON.withObject "ClientMessage/GQL_START" $
        \v -> (,,) <$> v .: "query"
                   <*> (v .: "variables" <|> pure HM.empty)
                   <*> v .:? "operationName"

theType :: (JSON.KeyValue kv) => T.Text -> kv
theType t = "type" .= t

instance JSON.ToJSON ServerMessage where
  toJSON (GQLConnectionError e)
    = JSON.object [theType "connection_error", "payload" .= e]
  toJSON GQLConnectionAck
    = JSON.object [theType "connection_ack"]
  toJSON (GQLData i p)
    = JSON.object [theType "data", "id" .= i, "payload" .= p]
  toJSON (GQLError i p)
    = JSON.object [theType "error", "id" .= i, "payload" .= p]
  toJSON (GQLComplete i)
    = JSON.object [theType "complete", "id" .= i]
  toJSON GQLKeepAlive
    = JSON.object [theType "ka"]

instance JSON.ToJSON Error where
  toJSON Error { message, locations, path }
    = JSON.object [ "message" .= message
                  , "locations" .= locations
                  , "path" .= path ]

instance JSON.ToJSON Location where
  toJSON Location { line, column }
    = JSON.object [ "line" .= line, "column" .= column ]

instance JSON.ToJSON Path where
  toJSON (Segment t) = JSON.toJSON t
  toJSON (Index   i) = JSON.toJSON i
