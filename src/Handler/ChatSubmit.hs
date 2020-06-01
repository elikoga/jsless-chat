{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ChatSubmit where

import Text.Hamlet (hamletFile)

import Import
import Handler.Chat.ChatHandlers

getChatSubmitR :: Handler Html
getChatSubmitR = do
    (formWidget, formEnctype) <- generateFormPost submitForm
    pc <- widgetToPageContent $(widgetFile "chatsubmit")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

postChatSubmitR :: Handler ()
postChatSubmitR = do
    ((res, formWidget), formEnctype) <- runFormPost submitForm
    case res of
        FormSuccess msg -> msgSendHandler msg
        _ -> return ()
    redirect ChatSubmitR

submitForm :: Form Text
submitForm = renderDivs $ areq textField fieldSettings Nothing
    where fieldSettings = FieldSettings { fsLabel = "Message"
                                        , fsTooltip = Nothing
                                        , fsId = Nothing
                                        , fsName = Just "Message"
                                        , fsAttrs = []
                                        }

