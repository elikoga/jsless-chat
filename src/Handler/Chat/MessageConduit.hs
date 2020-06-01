{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Chat.MessageConduit where

import qualified Data.Binary.Builder as Builder
import           Import
import           Text.Blaze.Html.Renderer.Pretty
import           Types.Message

messageChannelToConduit :: MonadIO m => TChan Message -> ConduitT i (Flush Builder.Builder) m ()
messageChannelToConduit chan = channelToConduit chan .| messageChannelToByteStringConduit .| byteStringToBuilderConduit .| flushingConduit

flushingConduit :: MonadIO m => ConduitT Builder.Builder (Flush Builder.Builder) m ()
flushingConduit = forever $ do
    maybeValue <- await
    case maybeValue of
        (Just value) -> do
            yield $ Chunk value
            yield Flush
        Nothing -> return ()

byteStringToBuilderConduit :: Monad m => ConduitT ByteString Builder.Builder m ()
byteStringToBuilderConduit = mapC Builder.fromByteString

messageChannelToByteStringConduit :: Monad m => ConduitT Message ByteString m ()
messageChannelToByteStringConduit = mapC messageToByteString

messageToByteString :: Message -> ByteString
messageToByteString msg = fromString $ renderHtml [shamlet|<p> <b>#{senderUsername msg}</b>: #{messageContent msg}|]

channelToConduit :: MonadIO m => TChan o -> ConduitT i o m ()
channelToConduit chan = repeatMC $ atomically $ readTChan chan