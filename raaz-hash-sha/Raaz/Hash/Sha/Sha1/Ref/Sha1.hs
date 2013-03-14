{-|

This module gives the reference implementation of the sha1
hash. Depending on your platform there might be a more efficient
implementation. So you /should not/ be using this code in production.

-}

{-# LANGUAGE TemplateHaskell #-}

module Raaz.Hash.Sha.Sha1.Ref.Sha1
       ( sha1CompressSingle
       ) where

import Control.Applicative

import Raaz.Types
import Raaz.Util.Ptr

import Raaz.Hash.Sha.Sha1.Type(SHA1(..))
import Raaz.Hash.Sha.Sha1.Ref.Sha1TH

-- | round functions generated from TH
$(round0f)
{-# INLINE round0 #-}
$(round1f)
{-# INLINE round1 #-}
$(round2f)
{-# INLINE round2 #-}
$(round3f)
{-# INLINE round3 #-}
$(round4f)
{-# INLINE round4 #-}

-- | Compresses one block.
sha1CompressSingle :: SHA1
                   -> CryptoPtr
                   -> IO SHA1
sha1CompressSingle (SHA1 h0 h1 h2 h3 h4) cptr = roundF h0 h1 h2 h3 h4
         <$> load cptr
         <*> loadFromIndex cptr 1
         <*> loadFromIndex cptr 2
         <*> loadFromIndex cptr 3
         <*> loadFromIndex cptr 4
         <*> loadFromIndex cptr 5
         <*> loadFromIndex cptr 6
         <*> loadFromIndex cptr 7
         <*> loadFromIndex cptr 8
         <*> loadFromIndex cptr 9
         <*> loadFromIndex cptr 10
         <*> loadFromIndex cptr 11
         <*> loadFromIndex cptr 12
         <*> loadFromIndex cptr 13
         <*> loadFromIndex cptr 14
         <*> loadFromIndex cptr 15
    where
      roundF a b c d e m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 =
          let (a',b',c',d',e') = round4 $ round3 $ round2 $ round1 $ round0
                (a,b,c,d,e,m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)
          in SHA1 (a+a') (b+b') (c+c') (d+d') (e+e')
