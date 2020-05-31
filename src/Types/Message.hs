{-# LANGUAGE NoImplicitPrelude #-}
module Types.Message where

import Import.NoFoundation

data Message = Message
    { senderUsername :: Text
    , messageContent  :: Text
    }