{-# LANGUAGE TemplateHaskell  #-}

module SearchBonds
    ( readMolecule
    , writeBonds
    , searchBonds
    ) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Label
import System.Directory (renameFile)
import System.IO.Unsafe
import System.IO

import qualified Data.Map as Map
import qualified System.IO.Strict as StrictIO

-- | НАЧАЛО. ОПИСАНИЕ ТИПОВ.
type CompAccur = Double
type ID        = Int
type Point     = (CompAccur, CompAccur, CompAccur)
type Element   = String

data Atom = Atom { _coordin  :: Point
                 , _element  :: Element
                 } deriving Show
type Molecule = Map.Map ID Atom

mkLabels [''Atom]
-- | КОНЕЦ. ОПИСАНИЕ ТИПОВ.

-- | НАЧАЛО. КОНСТРУКТОРЫ.
-- Простейшие конструкторы пустых значений типов

newAtom :: Atom
newAtom = Atom { _coordin = (0, 0, 0)
               , _element = []
               }

newMolecule :: Molecule
newMolecule = Map.empty

addAtom :: (ID, Atom) -> Molecule -> Molecule
addAtom (id, atom) molecule = Map.insert id atom molecule
-- | КОНЕЦ. КОНСТРУКТОРЫ.

-- | НАЧАЛА. Поиск близких атомов
searchBonds :: String -> String -> CompAccur -> Molecule -> [(ID, ID)]
searchBonds a b d molecule 
    | a == b    = searchBondsSame a d molecule
    | otherwise = searchBondsDiff a b d molecule

searchBondsSame :: String -> CompAccur -> Molecule -> [(ID, ID)]
searchBondsSame a d molecule =
    let w = Map.toList . Map.filter (\x -> get element x == a) $ molecule
        isNear a b = (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 <= d^2
                     where (x1, y1, z1) = get coordin a
                           (x2, y2, z2) = get coordin b
        searchBonds' (s:[]) = []
        searchBonds' ((k1, v1):s) = (map (\(k2, _) -> (k1, k2)) . filter (\(_, v2) -> isNear v1 v2) $ s) <> searchBonds' s
    in  searchBonds' w

searchBondsDiff :: String -> String -> CompAccur -> Molecule -> [(ID, ID)]
searchBondsDiff a b d molecule =
    let w = Map.toList . Map.filter (\x -> get element x `elem` [a, b]) $ molecule
        isNear a b = (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2 <= d^2
                     where (x1, y1, z1) = get coordin a
                           (x2, y2, z2) = get coordin b
        searchBonds' (s:[]) = []
        searchBonds' ((k1, v1):s) 
            | get element v1 == a = (map (\(k2, _) -> (k1, k2)) . filter (\(_, v2) -> isNear v1 v2) . filter (\(_, v2) -> get element v2 /= a) $ s) <> searchBonds' s
            | get element v1 == b = (map (\(k2, _) -> (k1, k2)) . filter (\(_, v2) -> isNear v1 v2) . filter (\(_, v2) -> get element v2 /= b) $ s) <> searchBonds' s
    in  searchBonds' w
-- | КОНЕЦ. Поиск близких атомов

-- | НАЧАЛО. ЧТЕНИЕ, ВЫВОД.
-- | Функция считывает молекулу из файла
readMolecule :: FilePath -> IO Molecule
readMolecule inf = return $ foldr addAtom newMolecule atoms
    where
        txt   = unsafePerformIO $ StrictIO.readFile inf
        atoms = [(id, Atom coordin elem) |
                line <- lines txt,
                let fields = words line
                    id      = read $ fields !! 1
                    elem    = take 1 $ fields !! 2
                    coordin = (read $ fields !! 6, read $ fields !! 7, read $ fields !! 8)
                   ]

writeBonds :: FilePath -> [(ID, ID)] -> IO ()
writeBonds ouf ab = do
    let ab_str = map (\(x,y) -> show x <> " " <> show y) ab
    (tmp_name, tmp_handle) <- openTempFile "." "temp"
    mapM_ (hPutStrLn tmp_handle) ab_str
    hClose tmp_handle
    renameFile tmp_name ouf

   