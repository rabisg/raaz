{-|

Some utility function for byte strings.

-}

{-# LANGUAGE FlexibleContexts #-}
module Raaz.Util.ByteString
       ( unsafeCopyToCryptoPtr
       , unsafeNCopyToCryptoPtr
       , length
       , hex, toHex
       ) where

import           Prelude hiding (length)
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString(ByteString)
import           Data.ByteString.Internal(toForeignPtr, memcpy, c2w, unsafeCreate)
import           Foreign.ForeignPtr(withForeignPtr)
import           Foreign.Ptr(castPtr, plusPtr)
import           Foreign.Storable(poke, peek)


import Raaz.Types

-- | A typesafe length for Bytestring
length :: ByteString -> BYTES Int
length = BYTES . B.length

-- | Copy the bytestring to the crypto buffer. This operation leads to
-- undefined behaviour if the crypto pointer points to an area smaller
-- than the size of the byte string.
unsafeCopyToCryptoPtr :: ByteString   -- ^ The source.
                      -> CryptoPtr    -- ^ The destination.
                      -> IO ()
unsafeCopyToCryptoPtr bs cptr =  withForeignPtr fptr $
           \ p -> memcpy dest (p `plusPtr` offset) (fromIntegral n)
    where (fptr, offset,n) = toForeignPtr bs
          dest = castPtr cptr


-- | Similar to `unsafeCopyToCryptoPtr` but takes an additional input
-- @n@ which is the number of bytes (expressed in type safe length
-- units) to transfer. This operation leads to undefined behaviour if
-- either the bytestring is shorter than @n@ or the crypto pointer
-- points to an area smaller than @n@.
unsafeNCopyToCryptoPtr :: CryptoCoerce n (BYTES Int)
                       => n              -- ^ length of data to be copied
                       -> ByteString     -- ^ The source byte string
                       -> CryptoPtr      -- ^ The buffer
                       -> IO ()
unsafeNCopyToCryptoPtr n bs cptr = withForeignPtr fptr $
           \ p -> memcpy dest (p `plusPtr` offset) (fromIntegral l)
    where (fptr, offset,_) = toForeignPtr bs
          dest    = castPtr cptr
          BYTES l = cryptoCoerce n :: BYTES Int

-- | Converts a crypto storable instances to its hexadecimal
-- representation.
toHex :: CryptoStore a => a -> ByteString
toHex = hex . toByteString

-- | Converts bytestring to hexadecimal representation.
hex :: ByteString -> ByteString
hex bs = unsafeCreate (2 * n) filler
  where (fptr, offset, n)      = toForeignPtr bs

        filler ptr = withForeignPtr fptr $
             \ bsPtr -> putBS (bsPtr `plusPtr` offset) 0 ptr

        putBS bsPtr i ptr
              | i < n     = do x <- peek bsPtr
                               put ptr x
                               putBS bsNewPtr (i+1) ptrNew
              | otherwise = return ()
          where bsNewPtr = bsPtr `plusPtr` 1
                ptrNew   = ptr `plusPtr`   2
        hexDigit x | x < 10    = c2w '0' + x
                   | otherwise = c2w 'a' + (x - 10)

        top4 x  = x `shiftR` 4
        bot4 x  = x  .&. 0x0F

        put ptr x = do poke ptr0 $ hexDigit $ top4 x
                       poke ptr1 $ hexDigit $ bot4 x
            where ptr0 = ptr
                  ptr1 = ptr `plusPtr` 1
