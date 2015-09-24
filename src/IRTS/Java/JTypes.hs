{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module IRTS.Java.JTypes where

import           Idris.Core.TT
import qualified Data.Text            as T
import           IRTS.Lang
import           Language.Java.Syntax
import qualified Language.Java.Syntax as J
import Data.Maybe

-----------------------------------------------------------------------
-- Primitive types

charType :: J.Type
charType =
  PrimType CharT

byteType :: J.Type
byteType = PrimType ByteT

shortType :: J.Type
shortType = PrimType ShortT

integerType :: J.Type
integerType = PrimType IntT

longType :: J.Type
longType = PrimType LongT

floatType :: J.Type
floatType = PrimType FloatT

doubleType :: J.Type
doubleType = PrimType DoubleT

array :: J.Type -> J.Type
array t = RefType . ArrayType $ t

addressType :: J.Type
addressType = longType

-----------------------------------------------------------------------
-- Boxed types

objectType :: J.Type
objectType =
  RefType . ClassRefType $ ClassType [(Ident "Object", [])]

bigIntegerType :: J.Type
bigIntegerType =
  RefType . ClassRefType $ ClassType [(Ident "BigInteger", [])]

stringType :: J.Type
stringType =
  RefType . ClassRefType $ ClassType [(Ident "String", [])]

bufferType :: J.Type
bufferType =
  RefType . ClassRefType $ ClassType [(Ident "ByteBuffer", [])]

threadType :: J.Type
threadType =
  RefType . ClassRefType $ ClassType [(Ident "Thread", [])]

callableType :: J.Type
callableType =
  RefType . ClassRefType $ ClassType [(Ident "Callable", [])]

voidType :: J.Type
voidType =
  RefType . ClassRefType $ ClassType [(Ident "Void", [])]

worldType :: J.Type
worldType =
  RefType . ClassRefType $ ClassType [(Ident "Object", [])]

userType :: String -> J.Type
userType name =
  RefType . ClassRefType $ ClassType [(Ident name, [])]

box :: J.Type -> J.Type
box (PrimType CharT  ) = RefType . ClassRefType $ ClassType [(Ident "Character", [])]
box (PrimType ByteT  ) = RefType . ClassRefType $ ClassType [(Ident "Byte", [])]
box (PrimType ShortT ) = RefType . ClassRefType $ ClassType [(Ident "Short", [])]
box (PrimType IntT   ) = RefType . ClassRefType $ ClassType [(Ident "Integer", [])]
box (PrimType LongT  ) = RefType . ClassRefType $ ClassType [(Ident "Long", [])]
box (PrimType DoubleT) = RefType . ClassRefType $ ClassType [(Ident "Double", [])]
box t = t

isFloating :: J.Type -> Bool
isFloating (PrimType DoubleT) = True
isFloating (PrimType FloatT)  = True
isFloating _ = False

isPrimitive :: J.Type -> Bool
isPrimitive (PrimType _) = True
isPrimitive _ = False

isArray :: J.Type -> Bool
isArray (RefType (ArrayType  _)) = True
isArray _ = False

isString :: J.Type -> Bool
isString (RefType (ClassRefType (ClassType [(Ident "String", _)]))) = True
isString _ = False

-----------------------------------------------------------------------
-- Idris rts classes

idrisClosureType :: J.Type
idrisClosureType =
  RefType . ClassRefType $ ClassType [(Ident "Closure", [])]

idrisTailCallClosureType :: J.Type
idrisTailCallClosureType =
  RefType . ClassRefType $ ClassType [(Ident "TailCallClosure", [])]

idrisObjectType :: J.Type
idrisObjectType =
  RefType . ClassRefType $ ClassType [(Ident "IdrisObject", [])]

foreignWrapperType :: J.Type
foreignWrapperType =
  RefType . ClassRefType $ ClassType [(Ident "ForeignWrapper", [])]

primFnType :: J.Type
primFnType =
  RefType . ClassRefType $ ClassType [(Ident "PrimFn", [])]

-----------------------------------------------------------------------
-- Java utility classes

arraysType :: J.Type
arraysType =
  RefType . ClassRefType $ ClassType [(Ident "Arrays", [])]

mathType :: J.Type
mathType =
  RefType . ClassRefType $ ClassType [(Ident "Math", [])]


-----------------------------------------------------------------------
-- Exception types

exceptionType :: J.Type
exceptionType =
  RefType . ClassRefType $ ClassType [(Ident "Exception", [])]

runtimeExceptionType :: J.Type
runtimeExceptionType =
  RefType . ClassRefType $ ClassType [(Ident "RuntimeException", [])]

-----------------------------------------------------------------------
-- Integer types

nativeTyToJType :: NativeTy -> J.Type
nativeTyToJType IT8  = byteType
nativeTyToJType IT16 = shortType
nativeTyToJType IT32 = integerType
nativeTyToJType IT64 = longType

intTyToJType :: IntTy -> J.Type
intTyToJType (ITFixed nt) = nativeTyToJType nt
intTyToJType (ITNative)   = integerType
intTyToJType (ITBig)      = bigIntegerType
intTyToJType (ITChar)     = charType
--intTyToJType (ITVec nt _) = array $ nativeTyToJType nt

arithTyToJType :: ArithTy -> J.Type
arithTyToJType (ATInt it) = intTyToJType it
arithTyToJType (ATFloat) = floatType

-----------------------------------------------------------------------
-- Context variables

localContextID :: Ident
localContextID = Ident "context"

localContext :: Exp
localContext = ExpName $ Name [localContextID]

globalContextID :: Ident
globalContextID = Ident "globalContext"

globalContext :: Exp
globalContext = ExpName $ Name [globalContextID]

newContextID :: Ident
newContextID = Ident "new_context"

newContext :: Exp
newContext = ExpName $ Name [newContextID]

contextArray :: LVar -> Exp
contextArray (Loc _) = localContext
contextArray (Glob _) = globalContext

contextParam :: FormalParam
contextParam = FormalParam [Final] (array objectType) False (VarId localContextID)

-----------------------------------------------------------------------
-- Constant types

constType :: Const -> J.Type
constType (I    _) = arithTyToJType (ATInt ITNative)
constType (BI   _) = arithTyToJType (ATInt ITBig   )
constType (Fl   _) = arithTyToJType (ATFloat       )
constType (Ch   _) = arithTyToJType (ATInt ITChar  )
constType (Str  _) = stringType
constType (B8   _) = arithTyToJType (ATInt $ ITFixed IT8 )
constType (B16  _) = arithTyToJType (ATInt $ ITFixed IT16)
constType (B32  _) = arithTyToJType (ATInt $ ITFixed IT32)
constType (B64  _) = arithTyToJType (ATInt $ ITFixed IT64)
constType (AType ty) = arithTyToJType ty
constType _        = objectType

-----------------------------------------------------------------------
-- Foreign types

isCType :: Idris.Core.TT.Name -> Bool
isCType (UN n ) = take 2 (str n) == "C_"

isJavaType :: Idris.Core.TT.Name -> Bool
isJavaType (UN n ) = take 5 (str n) == "Java_"

-- Currently we only support Java_* types for foreign functions
foreignType :: FDesc -> Maybe J.Type
foreignType (FCon t)
  | isCType t    = error ("Java backend does not (currently) support C calls")
  | isJavaType t = Just $ foreignType' t
  | otherwise    = error ("Java backend does not support " ++ show t)

-- TODO: We should really construct a user class reftype as we have
-- enough information
foreignType ty@(FApp (UN (T.unpack -> "Java_JavaT"))
                  [FApp (UN (T.unpack -> "JavaTyRef")) [FStr pck, FStr cl]]) =
  Just $ userType $ nameFromReturnType ty

foreignType (FApp t params)
  | isCType t            = error ("Java backend does not (currently) support for C calls")
  | sUN "Java_IntT" == t = Just $ foreignType' t'
  | otherwise            = error ("Java backend does not support " ++ show t)
  where
    FCon t' = head $ tail params

foreignType FUnknown = Nothing

foreignType fd = error ("foreignType: fdesc not implemented yet " ++ show fd)

foreignType' :: Idris.Core.TT.Name -> J.Type
foreignType' n
  | sUN "Java_Unit"      == n = voidType
  | sUN "Java_Str"       == n = stringType
  | sUN "Java_Ptr"       == n = objectType
                               
  | sUN "Java_IntNative" == n = integerType
  | sUN "Java_IntChar"   == n = charType
  | sUN "Java_IntBits8"  == n = byteType
  | sUN "Java_IntBits16" == n = shortType
  | sUN "Java_IntBits32" == n = integerType
  | sUN "Java_IntBits64" == n = longType
  | sUN "Java_Float"     == n = floatType
                               
  | sUN "Java_Any"      == n = objectType                           
  | otherwise                = error ("unimplemented FFI Java Type: " ++ show n)

-- the following two functions are used for calculating types of
-- parameters and return types for methods and allocating classes

-- calculate the types of the parameters, noting that the return type is
-- discarded

-- note: this is just for class definitions and so the type will arrive in a
-- certain way and thus we don't need to check all cases
paramTypes :: FDesc -> [J.Type]

paramTypes (FApp (UN (T.unpack -> "Java_FnT")) [FUnknown, t]) =
  paramTypes t

paramTypes (FApp (UN (T.unpack -> "Java_Fn")) (FUnknown:_:params)) =
  catMaybes $ handleParam params
  where
    -- we need to handle the result type special, as it can be a function or
    -- it can be the final result type of the method being defined

    handleParam [fapp@(FApp (UN (T.unpack -> "Java_FnIO")) _)] = []
    handleParam [fapp@(FApp (UN (T.unpack -> "Java_FnBase")) _)] = []
    
    handleParam [fapp@(FApp (UN (T.unpack -> "Java_Fn")) (FUnknown:_:_))] =
      map Just (paramTypes fapp)

    handleParam (x:xs) =
      foreignType x : handleParam xs

paramTypes fdesc = error ("unsupported type in paramTypes: " ++ show fdesc)

-- calculate the return type of a FFI type
nameFromReturnType :: FDesc -> String
nameFromReturnType (FApp (UN (T.unpack -> "Java_JavaT"))
                         [FApp (UN (T.unpack -> "JavaTyRef")) [FStr pck, FStr cl]]) =
  pck' ++ cl
  where
    pck' = if pck == "" then "" else pck ++ "."

nameFromReturnType (FApp (UN (T.unpack -> "Java_FnT")) [FUnknown, t]) = 
  nameFromReturnType t

nameFromReturnType (FApp (UN (T.unpack -> "Java_Fn")) params) =
  nameFromReturnType (head $ reverse params)

nameFromReturnType (FApp (UN (T.unpack -> "Java_FnIO")) params) =
    nameFromReturnType (head $ reverse params)

nameFromReturnType (FApp (UN (T.unpack -> "Java_FnBase")) params) =
    nameFromReturnType (head $ reverse params)

nameFromReturnType (FCon (UN (T.unpack -> "Java_Unit"))) = "void"
nameFromReturnType (FCon (UN (T.unpack -> "Java_Str"))) = "String"
nameFromReturnType (FCon (UN (T.unpack -> "Java_Ptr"))) = "Object"
nameFromReturnType (FCon (UN (T.unpack -> "Java_IntNative"))) = "int"
nameFromReturnType (FCon (UN (T.unpack -> "Java_IntChar"))) = "char"
nameFromReturnType (FCon (UN (T.unpack -> "Java_IntBits8"))) = "byte"
nameFromReturnType (FCon (UN (T.unpack -> "Java_IntBits16"))) = "short"
nameFromReturnType (FCon (UN (T.unpack -> "Java_IntBits32"))) = "int"
nameFromReturnType (FCon (UN (T.unpack -> "Java_IntBits64"))) = "long"
nameFromReturnType (FCon (UN (T.unpack -> "Java_Float"))) = "float"
nameFromReturnType (FCon (UN (T.unpack -> "Java_Any"))) = "Object"

nameFromReturnType n = error ("nameFromReturnType: unimplemented FFI: " ++ show n)
  
-----------------------------------------------------------------------
-- Primitive operation analysis

opName :: PrimFn -> String
opName x
  | (LSExt _ to)   <- x = "LSExt" ++ (suffixFor to)
  | (LZExt _ to)   <- x = "LZExt" ++ (suffixFor to)
  | (LTrunc _ to)  <- x = "LTrunc" ++ (suffixFor to)
  | (LFloatInt to) <- x = "LFloatInt" ++ (suffixFor to)
  | (LStrInt to)   <- x = "LStrInt" ++ (suffixFor to)
  | (LChInt to)    <- x = "LChInt" ++ (suffixFor to)
  | (LExternal si) <- x,
    si == sUN "prim__stdin"        = "LStdIn"
  | (LExternal si) <- x,
    si == sUN "prim__stdout"       = "LStdOut"
  | (LExternal si) <- x,
    si == sUN "prim__stderr"       = "LStdErr"
  | (LExternal si) <- x,                                
    si == sUN "prim__eqManagedPtr" = "LEqManagedPtr"                                
  | (LExternal si) <- x,                                
    si == sUN "prim__eqPtr"        = "LEqManagedPtr"
  | (LExternal si) <- x,                                                              
    si == sUN "prim__vm"           = "LVMPtr"
  | (LExternal si) <- x,                                                              
    si == sUN "prim__null"         = "LNull"
  | (LExternal si) <- x,                                                              
    si == sUN "prim__registerPtr"  = "LRegisterPtr"
  | (LExternal si) <- x,
    si == sUN "prim__readFile"     = "LReadFile"                                        
  | (LExternal si) <- x,
    si == sUN "prim__writeFile"    = "LWriteFile"       

  | otherwise = takeWhile ((/=) ' ') $ show x
  where
    suffixFor (ITFixed nt) = show nt
    suffixFor (ITNative) = show IT32
    suffixFor (ITBig) = show ITBig
    suffixFor (ITChar) = show ITChar

sourceTypes :: PrimFn -> [J.Type]
sourceTypes (LPlus from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LMinus from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LTimes from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LUDiv from) = [intTyToJType from, intTyToJType from]
sourceTypes (LSDiv from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LURem from) = [intTyToJType from, intTyToJType from]
sourceTypes (LSRem from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LAnd from) = [intTyToJType from, intTyToJType from]
sourceTypes (LOr from) = [intTyToJType from, intTyToJType from]
sourceTypes (LXOr from) = [intTyToJType from, intTyToJType from]
sourceTypes (LCompl from) = [intTyToJType from]
sourceTypes (LSHL from) = [intTyToJType from, intTyToJType from]
sourceTypes (LLSHR from) = [intTyToJType from, intTyToJType from]
sourceTypes (LASHR from) = [intTyToJType from, intTyToJType from]
sourceTypes (LEq from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LSLt from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LSLe from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LSGt from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LSGe from) = [arithTyToJType from, arithTyToJType from]
sourceTypes (LLt from) = [intTyToJType from, intTyToJType from]
sourceTypes (LLe from) = [intTyToJType from, intTyToJType from]
sourceTypes (LGt from) = [intTyToJType from, intTyToJType from]
sourceTypes (LGe from) = [intTyToJType from, intTyToJType from]
sourceTypes (LSExt from _) = [intTyToJType from]
sourceTypes (LZExt from _) = [intTyToJType from]
sourceTypes (LTrunc from _) = [intTyToJType from]
sourceTypes (LStrConcat) = repeat stringType
sourceTypes (LStrLt) = [stringType, stringType]
sourceTypes (LStrEq) = [stringType, stringType]
sourceTypes (LStrLen) = [stringType]
sourceTypes (LIntFloat from) = [intTyToJType from]
sourceTypes (LFloatInt _) = [doubleType]
sourceTypes (LIntStr from) = [intTyToJType from]
sourceTypes (LStrInt from) = [stringType]
sourceTypes (LFloatStr) = [doubleType]
sourceTypes (LStrFloat) = [stringType]
sourceTypes (LChInt _) = [charType]
sourceTypes (LIntCh from) = [intTyToJType from]
sourceTypes (LWriteStr) = [objectType, stringType]
sourceTypes (LReadStr) = [objectType]
sourceTypes (LFExp) = [doubleType]
sourceTypes (LFLog) = [doubleType]
sourceTypes (LFSin) = [doubleType]
sourceTypes (LFCos) = [doubleType]
sourceTypes (LFTan) = [doubleType]
sourceTypes (LFASin) = [doubleType]
sourceTypes (LFACos) = [doubleType]
sourceTypes (LFATan) = [doubleType]
sourceTypes (LFSqrt) = [doubleType]
sourceTypes (LFFloor) = [doubleType]
sourceTypes (LFCeil) = [doubleType]
sourceTypes (LStrHead) = [stringType]
sourceTypes (LStrTail) = [stringType]
sourceTypes (LStrCons) = [charType, stringType]
sourceTypes (LStrIndex) = [stringType, integerType]
sourceTypes (LStrRev) = [stringType]
sourceTypes (LSystemInfo) = [integerType]
sourceTypes (LFork) = [objectType]
sourceTypes (LPar) = [objectType]
sourceTypes (LNoOp) = repeat objectType

sourceTypes (LExternal n)  
  | n == sUN "prim__readFile"   = [worldType, objectType]
  | n == sUN "prim__writeFile"  = [worldType, objectType, stringType]                                  

  | n == sUN "prim__stdin"   = []                               
  | n == sUN "prim__stdout"  = []
  | n == sUN "prim__stderr"  = []

  -- see comment below on managed pointers                               
  | n == sUN "prim__eqManagedPtr" = [objectType, objectType]
  | n == sUN "prim__eqPtr" = [objectType, objectType]
  | n == sUN "prim__vm" = [threadType]
  | n == sUN "prim__null" = []
                            
-- @bgaster
-- i can't see any reason to support managed pointers in the Java
-- runtime, infact it seems to be counter to fact that Java is
-- managing our allocations and lifetimes. thus the runtime will raise
-- an exception if called
  | n == sUN "prim__registerPtr" = [objectType, integerType]
                             
  | otherwise = [] --error ("unsupported builtin: " ++ show n)

sourceTypes op = error ("non-suported op: " ++ show op)
-----------------------------------------------------------------------
-- Endianess markers

endiannessConstant :: Endianness -> Exp
endiannessConstant c =
  ExpName . Name . map Ident $ ["java", "nio", "ByteOrder", endiannessConstant' c]
  where
    endiannessConstant' BE                 = "BIG_ENDIAN"
    endiannessConstant' LE                 = "LITTLE_ENDIAN"
    endiannessConstant' (IRTS.Lang.Native) = endiannessConstant' BE

endiannessArguments :: PrimFn -> [Exp]
--endiannessArguments (LAppend _ end) = [endiannessConstant end]
--endiannessArguments (LPeek _ end)   = [endiannessConstant end]
endiannessArguments _               = []
