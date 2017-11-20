module Excelsior.Misc.Utils where

import Foundation

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap
