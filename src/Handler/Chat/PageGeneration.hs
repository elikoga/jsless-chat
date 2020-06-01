{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Chat.PageGeneration where

import qualified Data.List.Split as Split
import           Import
import           Text.Blaze.Html.Renderer.Pretty
import           Text.StringRandom

layoutPrefix :: (Widget -> Handler Html) -> Handler Text
layoutPrefix layout = do
    randomIdentifier <- liftIO $ stringRandomIO "[0-9A-z]{15}"
    site <- layout [whamlet|#{randomIdentifier}|]
    let siteString = renderHtml site
    let before:_ = Split.splitOn (unpack randomIdentifier) siteString
    return $ fromString before