{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}

module Twilio.Message where

import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Servant.API
import Data.Text (Text)

-------------------------------------------------------------------------------
-- * Message
-------------------------------------------------------------------------------

data Message = Message
  { msgTo   :: Text
  , msgFrom :: Text
  , msgBody :: Text
  } deriving (Show, Generic)

instance ToFormUrlEncoded Message where
  toFormUrlEncoded Message{..} =
    [ ("To",   msgTo)
    , ("From", msgFrom)
    , ("Body", msgBody)
    ]

-------------------------------------------------------------------------------
-- * Message Response
-------------------------------------------------------------------------------

data MessageResponse = MessageResponse
  { sid          :: Text
  , dateCreated  :: Text
  , dateUpdated  :: Text
  , dateSent     :: Maybe Text
  , accountSid   :: Text
  , to           :: Text
  , from         :: Text
  , body         :: Text
  , status       :: MessageStatus
  , numSegments  :: Text
  , numMedia     :: Text
  , direction    :: Text
  , apiVersion   :: Text
  , price        :: Maybe Text
  , priceUnit    :: Text
  , errorCode    :: Maybe ErrorCode
  , errorMessage :: Maybe Text
  , uri          :: Text
  } deriving (Show, Generic)

instance FromJSON MessageResponse where
  parseJSON (Object o) =
    MessageResponse
    <$> o .: "sid"
    <*> o .: "date_created"
    <*> o .: "date_updated"
    <*> o .: "date_sent"
    <*> o .: "account_sid"
    <*> o .: "to"
    <*> o .: "from"
    <*> o .: "body"
    <*> o .: "status"
    <*> o .: "num_segments"
    <*> o .: "num_media"
    <*> o .: "direction"
    <*> o .: "api_version"
    <*> o .: "price"
    <*> o .: "price_unit"
    <*> o .: "error_code"
    <*> o .: "error_message"
    <*> o .: "uri"
  parseJSON _ = mempty

-------------------------------------------------------------------------------
-- * Message Status
-------------------------------------------------------------------------------

data MessageStatus
  = Accepted
  | Queued
  | Sending
  | Sent
  | Receiving
  | Received
  | Delivered
  | Undelivered
  | Failed
  deriving Show

instance FromJSON MessageStatus where
  parseJSON (String str) = return $ case str of
    "accepted"    -> Accepted
    "queued"      -> Queued
    "sending"     -> Sending
    "sent"        -> Sent
    "receiving"   -> Receiving
    "received"    -> Received
    "delivered"   -> Delivered
    "undelivered" -> Undelivered
    "failed"      -> Failed
  parseJSON _ = mempty


-------------------------------------------------------------------------------
-- * Error Codes
-------------------------------------------------------------------------------

data ErrorCode
  = InvalidPhoneNumber
  | OtherErrorCode Int
    deriving (Show)

instance FromJSON ErrorCode where
  parseJSON (Number n) = return $ case n of
    -- Invalid 'To' Phone Number
    21211 -> InvalidPhoneNumber

    -- 'To' phone number cannot be reached
    21214 -> InvalidPhoneNumber

    -- Phone number does not appear to be valid
    21217 -> InvalidPhoneNumber

    _ -> OtherErrorCode (round n)

  parseJSON _ = mempty
  
-------------------------------------------------------------------------------
-- * Exceptions
-------------------------------------------------------------------------------

data TwilioException = TwilioException
  { exceptionStatus   :: Int
  , exceptionMessage  :: Text
  , exceptionCode     :: Maybe ErrorCode
  , exceptionMoreInfo :: Maybe Text
  } deriving Show

instance FromJSON TwilioException where
  parseJSON (Object o) =
    TwilioException
    <$> o .: "status"
    <*> o .: "message"
    <*> o .:? "code"
    <*> o .:? "more_info"
  parseJSON _ = mempty
