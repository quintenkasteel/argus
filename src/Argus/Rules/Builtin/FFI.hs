{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.FFI
-- Description : Foreign Function Interface rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for FFI, foreign imports, unsafe operations, and C interop.
-- Encourages safe FFI practices and proper resource handling.
--
-- == Rule Categories
--
-- * __Foreign Imports__: Foreign import declarations
-- * __Unsafe Operations__: Unsafe FFI patterns
-- * __Pointers__: Pointer and memory rules
-- * __Marshalling__: Data marshalling patterns

module Argus.Rules.Builtin.FFI
  ( -- * Rule Sets
    ffiRules
  , foreignImportRules
  , unsafeFFIRules
  , pointerRules
  , marshallingRules

    -- * Foreign Import Rules
  , foreignImportSafe
  , foreignImportUnsafe
  , foreignImportCapi
  , foreignExportStatic
  , foreignPrimitive
  , foreignInterruptible
  , ccallConvention
  , stdcallConvention

    -- * Unsafe FFI Rules
  , unsafePerformIO
  , unsafeInterleaveIO
  , unsafeDupablePerformIO
  , unsafeCoerce
  , unsafeFixIO
  , accursedUnutterablePerformIO
  , inlineUnsafePerformIO
  , unsafeIOToST

    -- * Pointer Rules
  , nullPtrCheck
  , ptrEquality
  , castPtrUnsafe
  , wordPtrConversion
  , funPtrCast
  , stablePtrFree
  , foreignPtrFinalize
  , mallocForeignPtr

    -- * Marshalling Rules
  , peekByteOff
  , pokeByteOff
  , allocaBytes
  , mallocBytes
  , freeBytes
  , withForeignPtr
  , withArrayLen
  , withCString

    -- * Rule Count
  , ffiRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All FFI-related rules.
ffiRules :: [Rule]
ffiRules = mconcat
  [ foreignImportRules
  , unsafeFFIRules
  , pointerRules
  , marshallingRules
  ]

-- | Total count of FFI rules.
ffiRuleCount :: Int
ffiRuleCount = length ffiRules

--------------------------------------------------------------------------------
-- Foreign Import Rules
--------------------------------------------------------------------------------

-- | Rules for foreign import declarations.
foreignImportRules :: [Rule]
foreignImportRules =
  [ foreignImportSafe
  , foreignImportUnsafe
  , foreignImportCapi
  , foreignExportStatic
  , foreignPrimitive
  , foreignInterruptible
  , ccallConvention
  , stdcallConvention
  ]

-- | Safe foreign import.
--
-- @
-- foreign import ccall safe "func" ...  -- Safe by default
-- @
foreignImportSafe :: Rule
foreignImportSafe =
  rule "foreign-import-safe" $
    matchText "foreign import.*safe"
    & category Security
    & severity Info
    & message "Foreign import marked safe - allows callbacks to Haskell"
    & safetyLevel ManualReview

-- | Unsafe foreign import.
--
-- @
-- foreign import ccall unsafe "func" ...  -- No Haskell callbacks
-- @
foreignImportUnsafe :: Rule
foreignImportUnsafe =
  rule "foreign-import-unsafe" $
    matchText "foreign import.*unsafe"
    & category Performance
    & severity Info
    & message "Unsafe foreign import - faster but no callbacks allowed"
    & note "Ensure foreign function doesn't call back to Haskell"
    & safetyLevel ManualReview

-- | CAPI calling convention.
--
-- @
-- foreign import capi "header.h func" ...  -- Use C header
-- @
foreignImportCapi :: Rule
foreignImportCapi =
  rule "foreign-import-capi" $
    matchText "foreign import capi"
    & category Style
    & severity Info
    & message "Using capi calling convention - good for portable C interop"
    & safetyLevel ManualReview

-- | Foreign export with static.
--
-- @
-- foreign export ccall "func" ...  -- Export to C
-- @
foreignExportStatic :: Rule
foreignExportStatic =
  rule "foreign-export-static" $
    matchText "foreign export"
    & category Style
    & severity Info
    & message "Exporting Haskell function to C"
    & safetyLevel ManualReview

-- | Primitive foreign import.
--
-- @
-- foreign import prim ...  -- GHC primitive
-- @
foreignPrimitive :: Rule
foreignPrimitive =
  rule "foreign-primitive" $
    matchText "foreign import prim"
    & category Style
    & severity Warning
    & message "Using primitive FFI - GHC-specific and low-level"
    & safetyLevel ManualReview

-- | Interruptible foreign call.
--
-- @
-- foreign import ccall interruptible ...  -- Can be interrupted
-- @
foreignInterruptible :: Rule
foreignInterruptible =
  rule "foreign-interruptible" $
    matchText "foreign import.*interruptible"
    & category Style
    & severity Info
    & message "Interruptible foreign call - can be interrupted by async exceptions"
    & safetyLevel ManualReview

-- | C calling convention.
--
-- @
-- foreign import ccall ...  -- C calling convention
-- @
ccallConvention :: Rule
ccallConvention =
  rule "ccall-convention" $
    matchText "foreign import ccall"
    & category Style
    & severity Info
    & message "Using C calling convention"
    & safetyLevel ManualReview

-- | Stdcall calling convention.
--
-- @
-- foreign import stdcall ...  -- Windows stdcall
-- @
stdcallConvention :: Rule
stdcallConvention =
  rule "stdcall-convention" $
    matchText "foreign import stdcall"
    & category Style
    & severity Info
    & message "Using stdcall convention - Windows-specific"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Unsafe FFI Rules
--------------------------------------------------------------------------------

-- | Rules for unsafe FFI patterns.
unsafeFFIRules :: [Rule]
unsafeFFIRules =
  [ unsafePerformIO
  , unsafeInterleaveIO
  , unsafeDupablePerformIO
  , unsafeCoerce
  , unsafeFixIO
  , accursedUnutterablePerformIO
  , inlineUnsafePerformIO
  , unsafeIOToST
  ]

-- | unsafePerformIO usage.
--
-- @
-- unsafePerformIO action  -- Escape IO
-- @
unsafePerformIO :: Rule
unsafePerformIO =
  rule "unsafePerformIO" $
    match ("unsafePerformIO _action" ==> "unsafePerformIO _action")
    & category Security
    & severity Warning
    & message "unsafePerformIO breaks referential transparency"
    & note "Ensure the IO action is truly pure"
    & safetyLevel ManualReview

-- | unsafeInterleaveIO usage.
--
-- @
-- unsafeInterleaveIO action  -- Lazy IO
-- @
unsafeInterleaveIO :: Rule
unsafeInterleaveIO =
  rule "unsafeInterleaveIO" $
    match ("unsafeInterleaveIO _action" ==> "unsafeInterleaveIO _action")
    & category Security
    & severity Warning
    & message "unsafeInterleaveIO creates lazy IO - can cause resource leaks"
    & safetyLevel ManualReview

-- | unsafeDupablePerformIO usage.
--
-- @
-- unsafeDupablePerformIO action  -- May duplicate
-- @
unsafeDupablePerformIO :: Rule
unsafeDupablePerformIO =
  rule "unsafeDupablePerformIO" $
    match ("unsafeDupablePerformIO _action" ==> "unsafeDupablePerformIO _action")
    & category Security
    & severity Warning
    & message "unsafeDupablePerformIO may duplicate IO action"
    & note "Only use for idempotent actions"
    & safetyLevel ManualReview

-- | unsafeCoerce usage.
--
-- @
-- unsafeCoerce x  -- Type cast without check
-- @
unsafeCoerce :: Rule
unsafeCoerce =
  rule "unsafeCoerce" $
    match ("unsafeCoerce _x" ==> "unsafeCoerce _x")
    & category Security
    & severity Error
    & message "unsafeCoerce bypasses type system - very dangerous"
    & note "Ensure source and target types have compatible representations"

-- | unsafeFixIO usage.
--
-- @
-- unsafeFixIO f  -- Unsafe fixpoint
-- @
unsafeFixIO :: Rule
unsafeFixIO =
  rule "unsafeFixIO" $
    match ("unsafeFixIO _f" ==> "unsafeFixIO _f")
    & category Security
    & severity Warning
    & message "unsafeFixIO - ensure function is sufficiently lazy"
    & safetyLevel ManualReview

-- | accursedUnutterablePerformIO usage.
--
-- @
-- accursedUnutterablePerformIO action  -- Most dangerous
-- @
accursedUnutterablePerformIO :: Rule
accursedUnutterablePerformIO =
  rule "accursedUnutterablePerformIO" $
    matchText "accursedUnutterablePerformIO"
    & category Security
    & severity Error
    & message "accursedUnutterablePerformIO - extremely unsafe, avoid at all costs"

-- | Inlined unsafePerformIO.
--
-- @
-- {-# NOINLINE myUnsafe #-}  -- Must use NOINLINE
-- @
inlineUnsafePerformIO :: Rule
inlineUnsafePerformIO =
  rule "inline-unsafePerformIO" $
    matchText "INLINE.*unsafePerformIO"
    & category Security
    & severity Error
    & message "INLINE with unsafePerformIO is unsafe - use NOINLINE"

-- | unsafeIOToST usage.
--
-- @
-- unsafeIOToST action  -- Convert IO to ST
-- @
unsafeIOToST :: Rule
unsafeIOToST =
  rule "unsafeIOToST" $
    match ("unsafeIOToST _action" ==> "unsafeIOToST _action")
    & category Security
    & severity Warning
    & message "unsafeIOToST escapes IO to ST - ensure safety"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Pointer Rules
--------------------------------------------------------------------------------

-- | Rules for pointer operations.
pointerRules :: [Rule]
pointerRules =
  [ nullPtrCheck
  , ptrEquality
  , castPtrUnsafe
  , wordPtrConversion
  , funPtrCast
  , stablePtrFree
  , foreignPtrFinalize
  , mallocForeignPtr
  ]

-- | Null pointer check.
--
-- @
-- if ptr == nullPtr then ...  -- Check for null
-- @
nullPtrCheck :: Rule
nullPtrCheck =
  rule "null-ptr-check" $
    matchText "== nullPtr|nullPtr =="
    & category Safety
    & severity Info
    & message "Checking for null pointer - good practice"
    & safetyLevel ManualReview

-- | Pointer equality.
--
-- @
-- ptr1 == ptr2  -- Compare pointers
-- @
ptrEquality :: Rule
ptrEquality =
  rule "ptr-equality" $
    matchText "Ptr.*==.*Ptr"
    & category Style
    & severity Info
    & message "Comparing pointers - ensure this is intentional"
    & safetyLevel ManualReview

-- | castPtr usage.
--
-- @
-- castPtr ptr  -- Unsafe type cast
-- @
castPtrUnsafe :: Rule
castPtrUnsafe =
  rule "castPtr-unsafe" $
    match ("castPtr _ptr" ==> "castPtr _ptr")
    & category Security
    & severity Warning
    & message "castPtr is unsafe - ensure types are compatible"
    & safetyLevel ManualReview

-- | WordPtr conversion.
--
-- @
-- ptrToWordPtr ptr  -- Convert to integer
-- @
wordPtrConversion :: Rule
wordPtrConversion =
  rule "wordPtr-conversion" $
    matchText "ptrToWordPtr|wordPtrToPtr"
    & category Style
    & severity Info
    & message "Converting between Ptr and WordPtr"
    & safetyLevel ManualReview

-- | FunPtr casting.
--
-- @
-- castFunPtr fptr  -- Cast function pointer
-- @
funPtrCast :: Rule
funPtrCast =
  rule "funPtr-cast" $
    match ("castFunPtr _fptr" ==> "castFunPtr _fptr")
    & category Security
    & severity Warning
    & message "Casting FunPtr is unsafe - ensure function signatures match"
    & safetyLevel ManualReview

-- | StablePtr freeing.
--
-- @
-- freeStablePtr sptr  -- Free stable pointer
-- @
stablePtrFree :: Rule
stablePtrFree =
  rule "stablePtr-free" $
    match ("freeStablePtr _sptr" ==> "freeStablePtr _sptr")
    & category Safety
    & severity Info
    & message "Freeing StablePtr - ensure no double-free"
    & safetyLevel ManualReview

-- | ForeignPtr finalization.
--
-- @
-- addForeignPtrFinalizer fin fptr  -- Add finalizer
-- @
foreignPtrFinalize :: Rule
foreignPtrFinalize =
  rule "foreignPtr-finalize" $
    matchText "addForeignPtrFinalizer"
    & category Style
    & severity Info
    & message "Adding ForeignPtr finalizer - ensure finalizer is safe"
    & safetyLevel ManualReview

-- | mallocForeignPtr usage.
--
-- @
-- mallocForeignPtr  -- Allocate with finalizer
-- @
mallocForeignPtr :: Rule
mallocForeignPtr =
  rule "mallocForeignPtr" $
    matchText "mallocForeignPtr"
    & category Style
    & severity Info
    & message "Using mallocForeignPtr - memory will be automatically freed"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Marshalling Rules
--------------------------------------------------------------------------------

-- | Rules for data marshalling.
marshallingRules :: [Rule]
marshallingRules =
  [ peekByteOff
  , pokeByteOff
  , allocaBytes
  , mallocBytes
  , freeBytes
  , withForeignPtr
  , withArrayLen
  , withCString
  ]

-- | peekByteOff pattern.
--
-- @
-- peekByteOff ptr off  -- Read at offset
-- @
peekByteOff :: Rule
peekByteOff =
  rule "peekByteOff" $
    match ("peekByteOff _ptr _off" ==> "peekByteOff _ptr _off")
    & category Safety
    & severity Info
    & message "Reading at byte offset - ensure offset is within bounds"
    & safetyLevel ManualReview

-- | pokeByteOff pattern.
--
-- @
-- pokeByteOff ptr off val  -- Write at offset
-- @
pokeByteOff :: Rule
pokeByteOff =
  rule "pokeByteOff" $
    matchText "pokeByteOff"
    & category Safety
    & severity Info
    & message "Writing at byte offset - ensure offset is within bounds"
    & safetyLevel ManualReview

-- | allocaBytes usage.
--
-- @
-- allocaBytes n $ \ptr -> ...  -- Stack allocation
-- @
allocaBytes :: Rule
allocaBytes =
  rule "allocaBytes" $
    matchText "allocaBytes"
    & category Safety
    & severity Info
    & message "Stack allocation with allocaBytes - don't return the pointer"
    & safetyLevel ManualReview

-- | mallocBytes usage.
--
-- @
-- mallocBytes n  -- Heap allocation
-- @
mallocBytes :: Rule
mallocBytes =
  rule "mallocBytes" $
    matchText "mallocBytes"
    & category Safety
    & severity Warning
    & message "Heap allocation - ensure memory is freed"
    & safetyLevel ManualReview

-- | free usage.
--
-- @
-- free ptr  -- Free memory
-- @
freeBytes :: Rule
freeBytes =
  rule "free-bytes" $
    matchText "\\bfree\\s"
    & category Safety
    & severity Info
    & message "Freeing memory - ensure no double-free or use-after-free"
    & safetyLevel ManualReview

-- | withForeignPtr pattern.
--
-- @
-- withForeignPtr fptr $ \ptr -> ...  -- Safe access
-- @
withForeignPtr :: Rule
withForeignPtr =
  rule "withForeignPtr" $
    match ("withForeignPtr _fptr _action" ==> "withForeignPtr _fptr _action")
    & category Style
    & severity Info
    & message "Using withForeignPtr - good practice for safe access"
    & safetyLevel ManualReview

-- | withArrayLen pattern.
--
-- @
-- withArrayLen arr $ \len ptr -> ...  -- Array with length
-- @
withArrayLen :: Rule
withArrayLen =
  rule "withArrayLen" $
    matchText "withArrayLen"
    & category Style
    & severity Info
    & message "Using withArrayLen - provides length and pointer"
    & safetyLevel ManualReview

-- | withCString pattern.
--
-- @
-- withCString str $ \cstr -> ...  -- Convert to C string
-- @
withCString :: Rule
withCString =
  rule "withCString" $
    match ("withCString _str _action" ==> "withCString _str _action")
    & category Style
    & severity Info
    & message "Converting Haskell string to CString"
    & safetyLevel ManualReview
