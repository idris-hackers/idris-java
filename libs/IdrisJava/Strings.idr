{- 
  Port of the FFI_C/IO functions found in Idris' Prelude.String to
  FFI_Java/JAVA_IO.
-} 
module IdrisJava.Strings

import IdrisJava

%access public

||| Check if a foreign pointer is null
partial
nullPtr : Ptr -> JAVA_IO Bool
nullPtr p = do ok <- invoke "isNull" (Ptr -> JAVA_IO Int) p
               return (ok /= 0)

||| Check if a supposed string was actually a null pointer
partial
nullStr : String -> JAVA_IO Bool
nullStr p = do ok <- invoke "isNull" (String -> JAVA_IO Int) p
               return (ok /= 0)
