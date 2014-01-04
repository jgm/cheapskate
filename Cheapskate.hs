{-# LANGUAGE OverloadedStrings #-}

module Cheapskate (markdown,
                   def,
                   walk,
                   walkM,
                   module Cheapskate.Types
                   ) where
import Cheapskate.Types
import Cheapskate.Parse
import Cheapskate.Render
import Data.Default (def)
import Data.Data
import Data.Generics.Uniplate.Data
import Text.Blaze.Html (ToMarkup(..))

instance ToMarkup Doc
  where toMarkup = renderDoc

walk :: (Data a, Data b) => (a -> a) -> (b -> b)
walk = transformBi

walkM :: (Data a, Data b, Monad m) => (a -> m a) -> (b -> m b)
walkM = transformBiM
