{-# LANGUAGE NoImplicitPrelude #-}
module Types.Message where

import Import.NoFoundation

data Message = Message
    { username :: Text
    , content  :: Text
    }