{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Chat where

import qualified Data.Binary.Builder as Builder

import qualified Data.Set as Set
import           Handler.Chat.ChatHandlers
import           Handler.Chat.MessageConduit
import           Handler.Chat.PageGeneration
import           Handler.Common
import           Import
import           Text.Blaze.Html.Renderer.Pretty
import           Types.Message

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

chatHandler :: Text -> TVar (Set Text) -> Handler TypedContent
chatHandler username loggedInTVar = do
    masterChannel <- getsYesod chatChan
    userChannel <- atomically $ dupTChan masterChannel
    pageText <- layoutPrefix defaultLayout
    urlRender <- getUrlRender
    respondSource "text/html" $
        bracketP
            (atomically $ modifyTVar loggedInTVar $ Set.insert username) -- before
            (const $ atomically $ modifyTVar loggedInTVar $ Set.delete username) -- after
            (const $ chatSiteConduit userChannel pageText urlRender) -- actually during

chatSiteConduit :: TChan Message -> Text -> (Route (HandlerSite (HandlerFor App)) -> Text)
                -> ConduitT () (Flush Builder.Builder) (HandlerFor site) ()
chatSiteConduit userChannel pageText urlRender = do
    sendChunk pageText
    sendChunkText "<div id=\"chat\">"
    sendChunk $ renderHtml [shamlet|<iframe src=#{urlRender ChatSubmitR}>|]
    sendChunkText "<div id=\"msgBox\">" -- To fill the buffer such that the browser immediately displays Contents.
    yieldMany $ join (replicate 2000 [Flush, Chunk $ Builder.fromByteString "<div></div>"] :: [[Flush Builder.Builder]])
    messageChannelToConduit userChannel

messageForm :: FormInput Handler Text
messageForm = ireq textField "Message"