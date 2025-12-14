{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module FFIExamples where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Unsafe FFI calls on main thread
--------------------------------------------------------------------------------

-- Should warn: unsafe FFI call may block runtime
foreign import ccall unsafe "unistd.h sleep"
  c_sleep_unsafe :: CUInt -> IO CUInt

-- Good: safe FFI call
foreign import ccall safe "unistd.h sleep"
  c_sleep_safe :: CUInt -> IO CUInt

--------------------------------------------------------------------------------
-- Missing error checking
--------------------------------------------------------------------------------

-- Should warn: no error checking on malloc result
allocateBuffer :: Int -> IO (Ptr CChar)
allocateBuffer size = mallocBytes size

-- Good: check for null pointer
allocateBufferSafe :: Int -> IO (Maybe (Ptr CChar))
allocateBufferSafe size = do
  ptr <- mallocBytes size
  if ptr == nullPtr
    then pure Nothing
    else pure (Just ptr)

--------------------------------------------------------------------------------
-- Resource leaks (missing free)
--------------------------------------------------------------------------------

-- Should warn: allocated memory not freed
processData :: IO String
processData = do
  ptr <- mallocBytes 100
  pokeByteOff ptr 0 (65 :: Word8)
  result <- peekCString (castPtr ptr)
  -- Missing: free ptr
  pure result

-- Good: properly frees memory
processDataSafe :: IO String
processDataSafe = do
  ptr <- mallocBytes 100
  pokeByteOff ptr 0 (65 :: Word8)
  result <- peekCString (castPtr ptr)
  free ptr
  pure result

--------------------------------------------------------------------------------
-- Prefer bracket for resource management
--------------------------------------------------------------------------------

-- Should suggest: use bracket/alloca pattern
manualAlloc :: IO Int
manualAlloc = do
  ptr <- malloc :: IO (Ptr CInt)
  poke ptr 42
  result <- peek ptr
  free ptr
  pure (fromIntegral result)

-- Good: using alloca
allocaPattern :: IO Int
allocaPattern = alloca $ \ptr -> do
  poke ptr 42
  result <- peek ptr
  pure (fromIntegral result)

--------------------------------------------------------------------------------
-- Unsafe coercions
--------------------------------------------------------------------------------

-- Should warn: unsafe pointer cast
unsafeCast :: Ptr CInt -> Ptr CDouble
unsafeCast = castPtr

-- Good: explicit conversion function
safeCast :: Ptr CInt -> IO (Ptr CDouble)
safeCast intPtr = do
  val <- peek intPtr
  doublePtr <- malloc
  poke doublePtr (fromIntegral val)
  pure doublePtr

--------------------------------------------------------------------------------
-- unsafePerformIO in FFI
--------------------------------------------------------------------------------

-- Should error: unsafePerformIO with side effects
pureFFI :: Int
pureFFI = unsafePerformIO $ do
  ptr <- malloc :: IO (Ptr CInt)
  poke ptr 42
  result <- peek ptr
  free ptr
  pure (fromIntegral result)

-- Good: proper IO type
ioFFI :: IO Int
ioFFI = do
  ptr <- malloc :: IO (Ptr CInt)
  poke ptr 42
  result <- peek ptr
  free ptr
  pure (fromIntegral result)

--------------------------------------------------------------------------------
-- CString handling
--------------------------------------------------------------------------------

-- Should warn: newCString without free
leakyString :: String -> IO CString
leakyString s = newCString s  -- caller must free, but not documented

-- Good: withCString for temporary use
withStringPattern :: String -> (CString -> IO a) -> IO a
withStringPattern = withCString

--------------------------------------------------------------------------------
-- Array bounds
--------------------------------------------------------------------------------

-- Should warn: no bounds checking
unsafeArrayAccess :: Ptr CInt -> Int -> IO CInt
unsafeArrayAccess arr idx = peekElemOff arr idx

-- Good: bounds-checked access
safeArrayAccess :: Ptr CInt -> Int -> Int -> IO (Maybe CInt)
safeArrayAccess arr len idx
  | idx < 0 || idx >= len = pure Nothing
  | otherwise = Just <$> peekElemOff arr idx

--------------------------------------------------------------------------------
-- Finalizers
--------------------------------------------------------------------------------

-- Should warn: missing finalizer for foreign pointer
createForeignPtr :: IO (ForeignPtr CInt)
createForeignPtr = do
  ptr <- malloc :: IO (Ptr CInt)
  poke ptr 0
  newForeignPtr_ ptr  -- No finalizer!

-- Good: with finalizer
createForeignPtrSafe :: IO (ForeignPtr CInt)
createForeignPtrSafe = do
  ptr <- malloc :: IO (Ptr CInt)
  poke ptr 0
  newForeignPtr finalizerFree ptr

--------------------------------------------------------------------------------
-- Storable alignment
--------------------------------------------------------------------------------

-- Should warn: unaligned access
unalignedAccess :: Ptr Word8 -> IO CInt
unalignedAccess ptr = peek (castPtr ptr)  -- May be unaligned

-- Good: ensure alignment
alignedAccess :: Ptr Word8 -> IO CInt
alignedAccess ptr = do
  let aligned = alignPtr (castPtr ptr) (alignment (undefined :: CInt))
  peek aligned
