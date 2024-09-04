{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatProtocol where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

data MessageType
    = Identify
    | Status
    | Users
    | Text
    | PublicText
    | NewRoom
    | Invite
    | JoinRoom
    | RoomUsers
    | RoomText
    | LeaveRoom
    | Disconnect
    | Response
    | NewUser
    | NewStatus
    | UserList
    | TextFrom
    | PublicTextFrom
    | JoinedRoom
    | RoomUserList
    | RoomTextFrom
    | LeftRoom
    | Disconnected
    deriving (Generic, Show)

instance FromJSON MessageType
instance ToJSON MessageType

data ChatMessage = ChatMessage
    { messageType :: MessageType
    , username    :: Maybe Text
    , roomname    :: Maybe Text
    , text        :: Maybe Text
    , status      :: Maybe Text
    , result      :: Maybe Text
    , extra       :: Maybe Text
    , users       :: Maybe [(Text, Text)]
    } deriving (Generic, Show)

instance FromJSON ChatMessage
instance ToJSON ChatMessage
