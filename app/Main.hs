{-# LANGUAGE BangPatterns     #-}
module Main where

import System.Environment
import SearchBonds

main :: IO ()
main = do
    args <- getArgs
    let !mol_fn = args !! 0
        !f_elem = args !! 1
        !s_elem = args !! 2
        !dist   = read $ args !! 3
        !result = args !! 4
    molecule <- readMolecule mol_fn
    let bonds = searchBonds f_elem s_elem dist molecule
    writeBonds result bonds

