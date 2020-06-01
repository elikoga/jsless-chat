{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Chat.PageGeneration where

import Import
import Text.StringRandom
import Text.Blaze.Html.Renderer.Pretty
import qualified Data.List.Split as Split

layoutPrefix :: (Widget -> Handler Html) -> Handler Text
layoutPrefix layout = do
    randomIdentifier <- liftIO $ stringRandomIO "[0-9A-z]{15}"
    site <- layout $ [whamlet|#{randomIdentifier}|]
    let siteString = renderHtml site
    let before:after:[] = Split.splitOn (unpack randomIdentifier) siteString
    return $ fromString before