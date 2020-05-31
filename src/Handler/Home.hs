{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Handler.Common
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

type Username = Text

getHomeR :: Handler Html
getHomeR = do
    mUsername <- lookupSession "username"
    (formWidget, formEnctype) <- generateFormPost $ usernameForm mUsername
    defaultLayout $ do
        setTitle "Welcome To The Chat!"
        $(widgetFile "homepage")

postHomeR :: Handler TypedContent
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost $ usernameForm Nothing
    case result of
        FormSuccess username -> loginHandler username
        _ -> redirect HomeR

usernameForm :: Maybe Text -> Form Username
usernameForm presetUsername = renderBootstrap3 BootstrapBasicForm $ areq textField textSettings presetUsername
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Username?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "Your Username")
                ]
            }