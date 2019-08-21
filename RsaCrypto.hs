module RsaCrypto (encrypt, decrypt,
                  stringToBlocks, blocksToString, genKeys) where

import Data.Bits
import Numeric
import Data.Char


data Block x = B [x]
  deriving (Eq, Ord, Read, Show)

createBlock :: Block x
createBlock = B []

blockToList :: Block x -> [x]
blockToList (B []) = []
blockToList (B xs) = xs

type Blocks a = [Block a]

partInBlocks :: [x] -> Int -> Blocks x
partInBlocks [] _ = []
partInBlocks xs n = (B (take n xs)) : partInBlocks (drop n xs) n

stringToBlocks :: String -> Blocks Integer
stringToBlocks [] = []
stringToBlocks xs = partInBlocks zs n
  where n         = minDiv (length xs)
        zs        = stringToNumber xs
        minDiv n
          | n<4       = n
          | otherwise = head [x | x <- [4..], mod n x == 0]

blocksToString :: Blocks Integer -> String
blocksToString [] = []
blocksToString xs = numberToString (concat (map blockToList xs))

stringToNumber :: String -> [Integer]
stringToNumber = map (toInteger.fromEnum)

numberToString :: [Integer] -> String
numberToString = map (toEnum.fromInteger)

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m    = 1
modExp b e m
  | testBit e 0 = (b `mod` m) * modExp ((b*b) `mod` m) (shiftR e 1) m `mod` m
  | otherwise   = modExp ((b*b) `mod` m) (shiftR e 1) m `mod` m

extGcd :: Integer -> Integer -> (Integer, Integer)
extGcd x 0     = (1, 0)
extGcd x y     = (t, s-q*t)
  where (q, r) = quotRem x y
        (s, t) = extGcd y r

reduce :: Integer -> Integer -> Integer
reduce a n
  | a >= n    = reduce (a-n) n
  | a < 0     = reduce (a+n) n
  | otherwise = a

invMod :: Integer -> Integer -> Integer
invMod x y = reduce (fst (extGcd x y)) y

compLn :: Integer -> Integer -> Integer
compLn p q = lcm (p-1) (q-1)

coprims :: Integer -> [Integer]
coprims 0 = []
coprims n = [toInteger e | e <- [3..(n-1)], (gcd n e) == 1]

type PrivateKey = (Integer, Integer, Integer, Integer, Integer, Integer)
type  PublicKey = (Integer, Integer)

genKeys :: Integer -> Integer -> (PublicKey, PrivateKey)
genKeys  p q = ((n, e), (p, q, d, dp, dq, qinv))
  where n    = p*q
        ln   = compLn p q
        e    = head (coprims ln)
        d    = invMod e (toInteger ln)
        dp   = mod d (p-1)
        dq   = mod d (q-1)
        qinv = invMod q p

encryptRsa :: PublicKey -> Integer -> Integer
encryptRsa (n, e) msg = modExp msg e n

decryptRsa :: PrivateKey -> Integer -> Integer
decryptRsa (p, q, d, dp, dq, qinv) msg = m2 + h*q
  where m1 = modExp msg dp p
        m2 = modExp msg dq q
        h  = mod (qinv * (m1 - m2)) p

encrypt :: PublicKey -> Blocks Integer -> Blocks Integer
encrypt pub_k xs = map (encryptBlock pub_k) xs
  where encryptBlock   _ (B [])  = B[]
        encryptBlock pb_k (B xs) = B (map (encryptRsa pb_k) xs)

decrypt :: PrivateKey -> Blocks Integer -> Blocks Integer
decrypt p_k xs = map (decryptBlock p_k) xs
  where decryptBlock  _ (B [])  = B []
        decryptBlock p_k (B xs) = B (map (decryptRsa p_k) xs)
