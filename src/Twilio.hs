{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Twilio
       ( -- * Endpoints
         message
         -- * Types
       , AccountSID (..)
       , AuthToken (..)
       , EncodedAuthToken (..)
       , Message (..)
       , MessageResponse (..)
       ) where

import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

import           Twilio.Message

newtype AccountSID = AccountSID
  { unAccountSID :: Text
  } deriving (Show, FromText, ToText)

newtype AuthToken = AuthToken
  { unAuthToken :: Text
  } deriving (Show, FromText)

newtype EncodedAuthToken = EncodedAuthToken
  { unEncodedAuthToken :: Text
  } deriving Show

instance ToText EncodedAuthToken where
  toText = ("Basic " <>) . unEncodedAuthToken

type TwilioAPI =
     Header "Authorization" EncodedAuthToken
  :> "2010-04-01"
  :> "Accounts"
  :> Capture "accountSID" AccountSID
  :> "Messages.json"
  :> ReqBody '[FormUrlEncoded] Message
  :> Post '[JSON] MessageResponse

api :: Proxy TwilioAPI
api = Proxy

message' :: Maybe EncodedAuthToken
         -> AccountSID
         -> Message
         -> EitherT ServantError IO MessageResponse

message' = client api (BaseUrl Https "api.twilio.com" 443)

message :: AuthToken
        -> AccountSID
        -> Message
        -> IO (Either ServantError MessageResponse)
message token account msg =
  runEitherT $ message' (Just $ creds account token) account msg

creds :: AccountSID -> AuthToken -> EncodedAuthToken
creds account token = EncodedAuthToken $
                      decodeUtf8 . B64.encode . encodeUtf8 $
                      (unAccountSID account <> ":" <> unAuthToken token)

