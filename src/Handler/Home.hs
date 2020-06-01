{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import Handler.Common
import Import
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