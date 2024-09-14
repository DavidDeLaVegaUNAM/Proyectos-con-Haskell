{-# LANGUAGE OverloadedStrings #-}

module Traductor where

import Data.Aeson (Value(..), (.:), withObject, FromJSON(..), Object)  
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import ProtocolClien (TipoMsg(..))  


parseListaUsuarios :: Value -> Parser [(Text, Text)]
parseListaUsuarios (Object obj) = mapM parsearClaveValor (KM.toList obj)
  where
    parsearClaveValor (k, v) = do
      let key = T.pack (show k)  
      val <- parseJSON v         
      return (key, val)
parseListaUsuarios _ = fail "Expected a JSON object"


