{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_oathbreaker (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "oathbreaker"
version :: Version
version = Version [0,1,1,0] []

synopsis :: String
synopsis = "Advanced RF Signal Processing and Threat Detection"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
