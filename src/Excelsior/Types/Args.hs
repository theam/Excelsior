{-# LANGUAGE ExistentialQuantification
           , ExtendedDefaultRules
           , FlexibleContexts
           , Rank2Types
           , ScopedTypeVariables
           , DeriveGeneric #-}
module Excelsior.Types.Args where

import Foundation
import System.FilePath (FilePath)
import Options.Generic

data Args = Args
    { inputDirectory  :: FilePath
    , outputDirectory :: Maybe FilePath
    }  deriving (Generic, Show)
instance ParseRecord Args
