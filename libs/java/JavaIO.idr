{-
  Desc: FFI interface for the Java backend.
  TODO: Add suppport for allocating classes
 -}
module JavaIO

namespace FFI_Java
  data JavaFn : Type -> Type where
       -- code generated can assume it's compiled just as 't'
       MkJavaFn : (x : t) -> JavaFn t

  -- Tell erasure analysis not to erase the argument
  %used MkJavaFn x

  ||| Supported Java primitive integer types
  data Java_IntTypes : Type -> Type where
       Java_IntChar   : Java_IntTypes Char
       Java_IntNative : Java_IntTypes Int
       Java_IntBits8  : Java_IntTypes Bits8
       Java_IntBits16 : Java_IntTypes Bits16
       Java_IntBits32 : Java_IntTypes Bits32
       Java_IntBits64 : Java_IntTypes Bits64

  ||| Supported Java foreign types
  data Java_Types : Type -> Type where
       Java_Str   : Java_Types String
       Java_Float : Java_Types Float
       Java_Ptr   : Java_Types Ptr
       Java_MPtr  : Java_Types ManagedPtr
       Java_Unit  : Java_Types ()
       Java_Any   : Java_Types (JavaFn a)
       Java_IntT  : Java_IntTypes i -> Java_Types i

  ||| A descriptor for the Java FFI. See the constructors of `Java_Types`
  ||| and `Java_IntTypes` for the concrete types that are available.
  FFI_Java : FFI
  FFI_Java = MkFFI Java_Types String String
  
JAVA_IO : Type -> Type
JAVA_IO = IO' FFI_Java
