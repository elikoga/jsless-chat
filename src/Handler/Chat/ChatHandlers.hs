{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Chat.ChatHandlers where

import Import
import Types.Message

msgSendHandler :: Text -> Handler ()
msgSendHandler msg = do
    mUsername <- lookupSession "username"
    case mUsername of
        Nothing -> noUserNameHandler
        Just username -> do
            masterChannel <- getsYesod chatChan
            let message = Message username msg
            atomically $ writeTChan masterChannel message
            return ()

noUserNameHandler :: Handler a
noUserNameHandler = do
    setMessage [shamlet|You haven't selected a username.|]
    redirect HomeR