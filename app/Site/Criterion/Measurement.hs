--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module Site.Criterion.Measurement where

--------------------------------------------------------------------------------

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Scientific
import           Data.String            (IsString, fromString)
import           Text.Printf            (printf)

--------------------------------------------------------------------------------

display :: (IsString a) => Double -> a
display = fromString . displayString

displayString :: Double -> String
displayString k
    | k < 0      = '-' : displayString (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u

--------------------------------------------------------------------------------
