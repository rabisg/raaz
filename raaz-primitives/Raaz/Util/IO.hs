{-

This module provides utility functions for taking input.

-}

{-# LANGUAGE FlexibleContexts         #-}

module Raaz.Util.IO
       ( getPass
       ) where

import Control.Monad (when)
import Control.Monad.State
import Control.Exception (bracket_)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (fromForeignPtr)
import Foreign.ForeignPtr.Safe ( ForeignPtr
                               , withForeignPtr
                               , castForeignPtr
                               )
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff)

import Raaz.Types
import Raaz.Util.Ptr
import Raaz.Util.SecureMemory

import System.IO


-- | Prompts the user for password and stores it in a secure memory
getPass :: CryptoCoerce size (BYTES Int)
        => String
        -> size
        -> StateT BookKeeper IO (Maybe ByteString)
getPass prompt size= do
  bkpr <- get
  mfpr <- lift $ allocSecureMem size bkpr
  let (BYTES size') = cryptoCoerce size
  case mfpr of
    Nothing   -> return Nothing
    Just fptr -> do
      len <- lift $ setEchoOff $ withForeignPtr fptr (flip getline size')
      return . Just $ fromForeignPtr (castForeignPtr fptr) 0 len
      where
        setEchoOff :: IO a -> IO a
        setEchoOff action = do
          putStr prompt
          hFlush stdout
          old <- hGetEcho stdin
          bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action
        getline :: CryptoPtr -> Int -> IO Int
        getline ptr 0   = getLine >> return 0
        getline ptr len = do
          hGetBuf stdin ptr 1
          c <- peekByteOff ptr 0 :: IO Char
          if ( c == '\n')
            then return 0
            else do
              len' <- getline (plusPtr ptr 1) (len - 1)
              return $ 1 + len'