module HuffmanCod (encodeHuff, decodeHuff) where

import Data.Char
import Data.List


data Key a b = ConsKey (a, b)
  deriving (Eq, Read, Show)

fstKey :: Key a b -> a
fstKey (ConsKey (x, y)) = x

sndKey :: Key a b -> b
sndKey (ConsKey (x, y)) = y

instance (Eq a, Eq b, Ord b) => Ord (Key a b) where
  ConsKey (c1, m) <= ConsKey (c2, n) = m <= n

data Map a = EmptyMap | NodeMap a (Map a) (Map a)
  deriving (Eq, Ord, Read, Show)

add :: (Eq a, Ord a, Num b) => a -> Map (Key a b) -> Map (Key a b)
add x EmptyMap = NodeMap (ConsKey (x, 1)) EmptyMap EmptyMap
add x (NodeMap (ConsKey (y, z)) iz dr)
  | x == y    = NodeMap (ConsKey (y, z+1)) iz dr
  | x < y     = NodeMap (ConsKey (y, z)) (add x iz) dr
  | otherwise = NodeMap (ConsKey (y, z)) iz (add x dr)

add' :: (Eq a, Ord a) => (Key a b) -> Map (Key a b) -> Map (Key a b)
add' z EmptyMap = NodeMap z EmptyMap EmptyMap
add' z (NodeMap t iz dr)
  | (fstKey z) < (fstKey t) = NodeMap t (add' z iz) dr
  | otherwise               = NodeMap t iz (add' z dr)
          
listToMap :: (Ord a, Eq a) => [Key a b] -> Map (Key a b)
listToMap = foldr add' EmptyMap

findMap :: (Ord a, Eq a, Ord b, Eq b) => a -> Map (Key a b) -> Key a b
findMap x (NodeMap (ConsKey (t, v)) h1 h2)
  | x == t    = (ConsKey (t, v))
  | x < t     = findMap x h1
  | otherwise = findMap x h2

mapOrder :: Map (Key Char Int) -> [Key Char Int]
mapOrder EmptyMap = []
mapOrder (NodeMap (ConsKey (x, y)) iz dr) =
  mapOrder iz ++ mapOrder dr ++ [ConsKey (x, y)] 

listToMapComb :: String -> Map (Key Char Int)
listToMapComb = foldr add EmptyMap

freqList :: String -> [Key Char Int]
freqList x = mapOrder (listToMapComb x)

data Abb a = EmptyAbb | NodeAbb a (Abb a) (Abb a)
  deriving (Eq, Ord, Show, Read)

abbEmpty :: Abb a
abbEmpty = EmptyAbb

addAbb :: Ord a => a -> Abb a -> Abb a
addAbb x EmptyAbb = NodeAbb x EmptyAbb EmptyAbb
addAbb x (NodeAbb y iz dr)
  | x <= y    = NodeAbb y (addAbb x iz) dr
  | otherwise = NodeAbb y iz (addAbb x dr)

inOrdenAbb :: Abb a -> [a]
inOrdenAbb      EmptyAbb     = []
inOrdenAbb (NodeAbb x iz dr) = inOrdenAbb iz ++ [x] ++ inOrdenAbb dr

sortTree :: Ord a => [a] -> [a]
sortTree x = inOrdenAbb (foldr addAbb abbEmpty x)

freqListOrdered :: String -> [Key Char Int]
freqListOrdered = sortTree . freqList


data HTree = Leaf (Key Char Int) | NodeHuff (Key Char Int) HTree HTree
  deriving (Eq, Read, Show)

instance Ord HTree where
  (Leaf h1) <= (Leaf h2)                         = (sndKey h1) <= (sndKey h2)               
  (NodeHuff k1 h11 h12) <= (NodeHuff k2 h21 h22) = (sndKey k1) <= (sndKey k2)               
  (Leaf h1) <= (NodeHuff k1 h11 h22)             = (sndKey h1) <= (sndKey k1)               
  (NodeHuff k1 h11 h22) <= (Leaf h1)             = (sndKey k1) <= (sndKey h1)

getKey :: HTree -> Int
getKey       (Leaf x)     = sndKey x
getKey (NodeHuff k h1 h2) = sndKey k

minAndRemove :: [HTree] -> (HTree, [HTree])
minAndRemove l = (minH, delete minH l)
  where   minH = minimum l

mergeInList :: [HTree] -> [HTree]
mergeInList l = (merge t1 t2) : s2
  where (t1, s1)  = minAndRemove l
        (t2, s2)  = minAndRemove s1
        merge x y = NodeHuff (ConsKey (' ', ((getKey x) + (getKey y)))) x y

buildHuffTree :: [HTree] -> HTree
buildHuffTree (x:[]) = x
buildHuffTree   xs   = buildHuffTree (mergeInList xs)

stringToHTree :: String -> HTree
stringToHTree = buildHuffTree . freqToNodesHTree . freqListOrdered
  where freqToNodesHTree = map Leaf

mapCodes :: String -> (HTree, Map (Key Char [Bool]))
mapCodes s = (hs, foldr add' EmptyMap (setCodes hs))
  where hs = stringToHTree s
        setCodes (Leaf (ConsKey (c, n))) = [ConsKey (c, [])]
        setCodes (NodeHuff (ConsKey (c, n)) h1 h2) =
          (map (\(ConsKey (x, y)) -> (ConsKey (x, True:y))) (setCodes h1)) ++
          (map (\(ConsKey (x, y)) -> (ConsKey (x, False:y))) (setCodes h2))

encodeHuff :: String -> ([Bool], (HTree, Map (Key Char [Bool])))
encodeHuff s = (encodeHuff' s (snd hf), hf)
  where hf = mapCodes s
        encodeHuff'   [] _    = []
        encodeHuff' (x:xs) hs = (sndKey (findMap x (snd hf))) ++
                                 encodeHuff' xs hs

decodeHuff :: ([Bool], (HTree, Map (Key Char [Bool]))) -> String
decodeHuff (code, (htree, maptree)) = find' code htree []
  where find' [] (Leaf k) xs = xs ++ [(fstKey k)]
        find' cs (Leaf k) xs = find' cs htree (xs ++ [(fstKey k)])
        find' (c:cs) (NodeHuff k iz dr) xs
          | c == True = find' cs iz xs
          | otherwise = find' cs dr xs
