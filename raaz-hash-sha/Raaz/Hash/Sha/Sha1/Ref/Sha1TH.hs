{-

This moduler contains helper template haskell functions which are
spliced in the actual module.

-}

{-# LANGUAGE TemplateHaskell #-}

module Raaz.Hash.Sha.Sha1.Ref.Sha1TH
       ( round0f, round1f, round2f, round3f, round4f
       ) where

import Control.Applicative
import Data.Bits
import Language.Haskell.TH

import Raaz.Types
import Raaz.Util.TH

-- | 0 - 15
round0f :: DecsQ
round0f = sequence $ [typeSig, funD name [cls]]
  where
    name = mkName "round0"
    cls = clause [tupP $ args1 ++ args2] (normalB (LetE <$> step <*>
                                                   tupE (out1 ++ out2))) []
    args1 = map (flip subP (-1 :: Int)) ["a","b","c","d","e"]
    args2 = map (subP "w") [0..15 :: Int]
    out1 = map (flip subE (15 :: Int)) ["a","b","c","d","e"]
    out2 = map (subE "w") [16..31 :: Int]
    step = declarations [wdecs,adecs,cdecs,restdecs] [0..15]
    typeSig = sigD name $ appT (appT arrowT tup) tup
    tup = foldl (\x _ -> appT x wordtype) (tupleT 21) [1..21 :: Int]
    wordtype = (conT ''Word32BE)

-- | 16 - 31
round1f :: DecsQ
round1f = sequence $ [typeSig, funD name [cls]]
  where
    name = mkName "round1"
    cls = clause [tupP $ args1 ++ args2] (normalB (LetE <$> step <*>
                                                   tupE (out1 ++ out2))) []
    args1 = map (flip subP (15 :: Int)) ["a","b","c","d","e"]
    args2 = map (subP "w") [16..31 :: Int]
    out1 = map (flip subE (31 :: Int)) ["a","b","c","d","e"]
    out2 = map (subE "w") [32..47 :: Int]
    step = declarations [wdecs,adecs,cdecs,restdecs] [16..31]
    typeSig = sigD name $ appT (appT arrowT tup) tup
    tup = foldl (\x _ -> appT x wordtype) (tupleT 21) [1..21 :: Int]
    wordtype = (conT ''Word32BE)

-- | 32 - 47
round2f :: DecsQ
round2f = sequence $ [typeSig, funD name [cls]]
  where
    name = mkName "round2"
    cls = clause [tupP $ args1 ++ args2] (normalB (LetE <$> step <*>
                                                   tupE (out1 ++ out2))) []
    args1 = map (flip subP (31 :: Int)) ["a","b","c","d","e"]
    args2 = map (subP "w") [32..47 :: Int]
    out1 = map (flip subE (47 :: Int)) ["a","b","c","d","e"]
    out2 = map (subE "w") [48..63 :: Int]
    step = declarations [wdecs,adecs,cdecs,restdecs] [32..47]
    typeSig = sigD name $ appT (appT arrowT tup) tup
    tup = foldl (\x _ -> appT x wordtype) (tupleT 21) [1..21 :: Int]
    wordtype = (conT ''Word32BE)


-- | 48 - 63
round3f :: DecsQ
round3f = sequence $ [typeSig, funD name [cls]]
  where
    name = mkName "round3"
    cls = clause [tupP $ args1 ++ args2] (normalB (LetE <$> step <*>
                                                   tupE (out1 ++ out2))) []
    args1 = map (flip subP (47 :: Int)) ["a","b","c","d","e"]
    args2 = map (subP "w") [48..63 :: Int]
    out1 = map (flip subE (63 :: Int)) ["a","b","c","d","e"]
    out2 = map (subE "w") [64..79 :: Int]
    step = declarations [wdecs,adecs,cdecs,restdecs] [48..63]
    typeSig = sigD name $ appT (appT arrowT tup) tup
    tup = foldl (\x _ -> appT x wordtype) (tupleT 21) [1..21 :: Int]
    wordtype = (conT ''Word32BE)

-- | 64 - 79
round4f :: DecsQ
round4f = sequence $ [typeSig,funD name [cls]]
  where
    name = mkName "round4"
    cls = clause [tupP $ args1 ++ args2] (normalB (LetE <$> step <*>
                                                   tupE out1)) []
    args1 = map (flip subP (63 :: Int)) ["a","b","c","d","e"]
    args2 = map (subP "w") [64..79 :: Int]
    out1 = map (flip subE (79 :: Int)) ["a","b","c","d","e"]
    step = declarations [adecs,cdecs,restdecs] [64..79]
    typeSig = sigD name $ appT (appT arrowT tup) tupout
    tup = foldl (\x _ -> appT x wordtype) (tupleT 21) [1..21 :: Int]
    tupout = foldl (\x _ -> appT x wordtype) (tupleT 5) [1..5 :: Int]
    wordtype = (conT ''Word32BE)

adecs :: Int -> DecsQ
adecs = variable' "a" ''Word32BE body
  where
    body j = [| $(r') + f j $(b $ j-1) $(c $ j-1) $(d $ j-1) + $(e $ j-1) + $(k j) + $(w j) :: Word32BE |]
      where r' = [| rotateL $(a $ j-1) 5 |]

cdecs :: Int -> DecsQ
cdecs = variable' "c" ''Word32BE body
  where
    body j = [| rotateL $(b $ j-1) 30 |]

wdecs :: Int -> DecsQ
wdecs i = variable' "w" ''Word32BE body (i+16)
  where
    body j = [| rotateL ($(w $ j-3)  `xor` $(w $ j-8)  `xor`
                         $(w $ j-14) `xor` $(w $ j-16)) 1 |]

k :: Int -> ExpQ
k j | j <= 19    = [| 0x5a827999 :: Word32BE |]
    | j <= 39    = [| 0x6ed9eba1 :: Word32BE |]
    | j <= 59    = [| 0x8f1bbcdc :: Word32BE |]
    | otherwise  = [| 0xca62c1d6 :: Word32BE |]

restdecs :: Int -> DecsQ
restdecs = permute [("e","d"),("d","c"),("b","a")]

-- | The round functions
f :: Int -> Word32BE -> Word32BE -> Word32BE -> Word32BE
{-# INLINE f #-}
f i x y z | i <= 19   = ch x y z
          | i <= 39   = parity x y z
          | i <= 59   = maj x y z
          | otherwise = parity x y z

ch :: Word32BE -> Word32BE -> Word32BE -> Word32BE
{-# INLINE ch #-}
ch x y z = (x .&. y) `xor` (complement x .&. z)

parity :: Word32BE -> Word32BE -> Word32BE -> Word32BE
{-# INLINE parity #-}
parity x y z = x `xor` y `xor` z

-- same as (x .&. y) `xor` (y .&. z) `xor` (z .&. x)
maj :: Word32BE -> Word32BE -> Word32BE -> Word32BE
{-# INLINE maj #-}
maj x y z = (x .&. (y .|. z)) .|. (y .&. z)


a,b,c,d,e,w :: Int -> ExpQ
a = subE "a"
b = subE "b"
c = subE "c"
d = subE "d"
e = subE "e"
w = subE "w"
