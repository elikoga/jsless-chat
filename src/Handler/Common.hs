{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    return $ TypedContent "image/x-icon" $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain $ toContent $(embedFile "config/robots.txt")

loginHandler :: Text -> Handler a
loginHandler username = do
    loggedInTVar <- getsYesod loggedInUsers
    loggedIn <- readTVarIO loggedInTVar
    if notMember username loggedIn
        then do
        setSession "username" username
        redirect ChatR
        else alreadyLoggedInHandler

alreadyLoggedInHandler :: Handler a
alreadyLoggedInHandler = do
    setMessage [shamlet|Someone with that username is alread logged in.|]
    redirect HomeR