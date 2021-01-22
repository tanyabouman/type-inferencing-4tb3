module Main where

import qualified BasicInference (main)
import qualified TypeVariables (main)

main :: IO ()
main = do
  BasicInference.main
  TypeVariables.main

