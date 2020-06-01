{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Chat where

import Import

import Handler.Common
import Handler.Chat.MessageConduit
import Handler.Chat.PageGeneration
import Types.Message
import Text.Blaze.Html.Renderer.Pretty
import Conduit
import qualified Data.Binary.Builder as Builder
import qualified Data.Set as Set

getChatR :: Handler TypedContent
getChatR = do
    mUsername <- lookupSession "username"
    case mUsername of
        Nothing -> noUserNameHandler
        Just username -> do
            loggedInTVar <- getsYesod loggedInUsers
            loggedIn <- readTVarIO loggedInTVar
            if member username loggedIn
                then alreadyLoggedInHandler
                else chatHandler username loggedInTVar

postChatR :: Handler ()
postChatR = do
    res <- runInputPostResult messageForm
    case res of
        FormSuccess msg -> msgSendHandler msg
        _ -> return ()

noUserNameHandler :: Handler a
noUserNameHandler = do
    setMessage [shamlet|You haven't selected a username.|]
    redirect HomeR

chatHandler :: Text -> TVar (Set Text) -> Handler TypedContent
chatHandler username loggedInTVar = do
    masterChannel <- getsYesod chatChan
    userChannel <- atomically $ dupTChan masterChannel
    pageText <- layoutPrefix defaultLayout
    urlRender <- getUrlRender

    respondSource "text/html" $ bracketP
        (atomically $ modifyTVar loggedInTVar $ Set.insert username)            -- before
        (const $ atomically $ modifyTVar loggedInTVar $ Set.delete username)    -- after
        $ const $ chatSiteConduit userChannel pageText urlRender                -- actually during

chatSiteConduit :: TChan Message -> Text -> (Route (HandlerSite (HandlerFor App)) -> Text) -> ConduitT () (Flush Builder.Builder) (HandlerFor site) ()
chatSiteConduit userChannel pageText urlRender = do
        sendChunk pageText
        sendChunk $ renderHtml [shamlet|<iframe name="dummyframe" id="dummyframe" style="display: none;">"|]
        sendChunkText "<div id=\"chat\">"
        sendChunk $ renderHtml $ [hamlet|<form method="post" action="#{urlRender ChatR}" target="dummyframe">
    <input name="Message" type="text" required value>
    <button class="btn btn-primary" type="submit">
        Send!
    <button class="btn btn-primary" type="reset">|] ()
        sendChunkText "<div id=\"msgBox\">"

        -- To fill the buffer such that the browser immediately displays Contents.
        yieldMany $ join (replicate 200 [Flush, Chunk $ Builder.fromByteString "<div></div>"] :: [[Flush Builder.Builder]])

        messageChannelToConduit userChannel


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

messageForm :: FormInput Handler Text
messageForm = ireq textField "Message"