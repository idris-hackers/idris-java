module IdrisJava

%access public

namespace FFI_Java

  ||| The universe of foreign Java types.
  data JavaTy =
    ||| a foreign reference type
    JavaTyRef String String |
    ||| a foreign value type
    JavaTyVal String String |
    ||| a foreign array type
    JavaTyArr JavaTy

  instance Eq JavaTy where
    (JavaTyRef ns t) == (JavaTyRef ns' t') = ns == ns' && t == t'
    (JavaTyVal ns t) == (JavaTyVal ns' t') = ns == ns' && t == t'
    _                == _                  = False

  ||| A foreign descriptor.
  data JavaForeign =
    ||| Read the named static field of the given foreign type.
    JavaReadField String |
    ||| With the named field of the given foreign type.
    JavaWriteField String |
    ||| Call the static method of the given foreign type.
    JavaInvoke String |
    ||| Call the named method of the given foreign type.
    JavaInvokeDyn String |
    ||| Call a constructor
    JavaNew |
    ||| Allocate a anonymous class
    JavaNewAnonymous String |
    ||| Export a function under the given name.    
    JavaExport String |
    ||| Export a function under its original name.
    JavaDefault   

  mutual
    data JavaFn : Type -> Type where
       -- code generated can assume it's compiled just as 't'
       MkJavaFn : (x : t) -> JavaFn t

    data Java   : JavaTy -> Type where
       MkJava : (ty : JavaTy) -> Java ty

    ||| Supported Java primitive integer types
    data Java_IntTypes : Type -> Type where
       Java_IntChar   : Java_IntTypes Char
       Java_IntNative : Java_IntTypes Int
       Java_IntBits8  : Java_IntTypes Bits8
       Java_IntBits16 : Java_IntTypes Bits16
       Java_IntBits32 : Java_IntTypes Bits32
       Java_IntBits64 : Java_IntTypes Bits64

    data Java_FnTypes : Type -> Type where
       Java_Fn     : Java_Types s -> Java_FnTypes t -> Java_FnTypes (s -> t)
       Java_FnIO   : Java_Types t -> Java_FnTypes (IO' l t)
       Java_FnBase : Java_Types t -> Java_FnTypes t

    ||| Supported Java foreign types
    data Java_Types : Type -> Type where
       Java_Str   : Java_Types String
       Java_Float : Java_Types Float
       Java_Ptr   : Java_Types Ptr
       Java_Unit  : Java_Types ()
       Java_JavaT : Java_Types (Java a)
       Java_FnT   : Java_FnTypes a -> Java_Types (JavaFn a)
       Java_IntT  : Java_IntTypes i -> Java_Types i

  ||| A descriptor for the Java FFI. See the constructors of `Java_Types`
  ||| and `Java_IntTypes` for the concrete types that are available.
  FFI_Java : FFI
  FFI_Java = MkFFI Java_Types JavaForeign String

-- Tell erasure analysis not to erase the argument
%used MkJavaFn x


JAVA_IO : Type -> Type
JAVA_IO = IO' FFI_Java

-- Some routines for calling methods, allocating class instances, and
-- setting and reading fields.

refty : String -> String -> Type
refty pck cl = Java (JavaTyRef pck cl)

%inline
invoke : (fname : String) -> (ty : Type) ->
         {auto fty : FTy FFI_Java [] ty} -> ty
invoke fname ty = foreign FFI_Java (JavaInvoke fname) ty 

%inline
invokedyn : (fname : String) -> (ty : Type) -> 
            {auto fty : FTy FFI_Java [] ty} -> ty
invokedyn fname ty = foreign FFI_Java (JavaInvokeDyn fname) ty 

%inline
read : (field : String) -> (ty : Type) ->
       {auto fty : FTy FFI_Java [] ty} -> ty
read field ty = foreign FFI_Java (JavaReadField field) ty

%inline
new : (ty : Type) -> {auto fty : FTy FFI_Java [] ty} -> ty
new ty = foreign FFI_Java JavaNew ty

%inline 
newanonymous : String -> (ty : Type) -> {auto fty : FTy FFI_Java [] ty} -> ty
newanonymous name ty = foreign FFI_Java (JavaNewAnonymous name) ty

class IsA a b where {}
