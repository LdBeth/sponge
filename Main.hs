module Main where

import           System.Environment       (getArgs)

import qualified Sponge as S (sponge)

main :: IO ()
main = getArgs >>= S.sponge
