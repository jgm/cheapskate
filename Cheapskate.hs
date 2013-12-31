{-# LANGUAGE OverloadedStrings #-}

module Cheapskate (parseMarkdown,
                   renderBlocks,
                   RenderOptions(..),
                   def,
                   module Cheapskate.Types) where
import Cheapskate.Types
import Cheapskate.Parse
import Cheapskate.Render
import Data.Default (def)
