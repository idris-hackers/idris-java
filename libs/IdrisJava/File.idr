{- 
  Port of the FFI_C/IO functions found in Idris' Prelude.File to
  FFI_Java/JAVA_IO.
-} 
module IdrisJava.File

import IdrisJava
import IdrisJava.Strings

%access public

||| A file handle
abstract
data JFile = FHandle Ptr

||| Standard input
stdin : JFile
stdin = FHandle prim__stdin

||| Standard output
stdout : JFile
stdout = FHandle prim__stdout

||| Standard output
stderr : JFile
stderr = FHandle prim__stderr

||| Call the RTS's file opening function
do_fopen : String -> String -> JAVA_IO Ptr
do_fopen f m
   = invoke "fileOpen" (String -> String -> JAVA_IO Ptr) f m

||| Open a file
||| @ f the filename
||| @ m the mode as a String (`"r"`, `"w"`, or `"r+"`)
fopen : (f : String) -> (m : String) -> JAVA_IO JFile
fopen f m = do h <- do_fopen f m
               return (FHandle h)

partial
validFile : JFile -> JAVA_IO Bool
validFile (FHandle h) = do x <- nullPtr h
                           return (not x)

||| Open a file
||| @ f the filename
||| @ m the mode
partial
openFile : (f : String) -> (m : Mode) -> JAVA_IO JFile
openFile f m = fopen f (modeStr m) where
  modeStr Read  = "r"
  modeStr Write = "w"
  modeStr ReadWrite = "r+"


partial
do_fclose : Ptr -> JAVA_IO ()
do_fclose h = invoke "fileClose" (Ptr -> JAVA_IO ()) h

partial
closeFile : JFile -> JAVA_IO ()
closeFile (FHandle h) = do_fclose h

-- do_fread is already lifted to IO' l String

fgetc : JFile -> JAVA_IO Char
fgetc (FHandle h) = return (cast !(invoke "fgetc" (Ptr -> JAVA_IO Int) h))

fgetc' : JFile -> JAVA_IO (Maybe Char)
fgetc' (FHandle h)
   = do x <- invoke "fgetc" (Ptr -> JAVA_IO Int) h
        if (x < 0) then return Nothing
                   else return (Just (cast x))

fflush : JFile -> JAVA_IO ()
fflush (FHandle h) = invoke "fflush" (Ptr -> JAVA_IO ()) h

do_popen : String -> String -> JAVA_IO Ptr
do_popen f m = invoke "do_popen" (String -> String -> JAVA_IO Ptr) f m

popen : String -> Mode -> JAVA_IO JFile
popen f m = do ptr <- do_popen f (modeStr m)
               return (FHandle ptr)
  where
    modeStr Read  = "r"
    modeStr Write = "w"
    modeStr ReadWrite = "r+"

pclose : JFile -> JAVA_IO ()
pclose (FHandle h) = invoke "pclose" (Ptr -> JAVA_IO ()) h

partial
fread : JFile -> IO' l String
fread (FHandle h) = do_fread h

partial
do_fwrite : Ptr -> String -> JAVA_IO ()
do_fwrite h s = do prim_fwrite h s
                   return ()
                   
partial
fwrite : JFile -> String -> JAVA_IO ()
fwrite (FHandle h) s = do_fwrite h s                 

partial
do_feof : Ptr -> JAVA_IO Int
do_feof h = invoke "fileEOF" (Ptr -> JAVA_IO Int) h

||| Check if a file handle has reached the end
feof : JFile -> JAVA_IO Bool
feof (FHandle h) = do eof <- do_feof h
                      return (not (eof == 0))


partial
do_ferror : Ptr -> JAVA_IO Int
do_ferror h = invoke "fileError" (Ptr -> JAVA_IO Int) h

ferror : JFile -> JAVA_IO Bool
ferror (FHandle h) = do err <- do_ferror h
                        return (not (err == 0))

fpoll : JFile -> JAVA_IO Bool
fpoll (FHandle h) = do p <- invoke "fpoll" (Ptr -> JAVA_IO Int) h
                       return (p > 0)

||| Read the contents of a file into a string
partial -- no error checking!
readFile : String -> JAVA_IO String
readFile fn = do h <- openFile fn Read
                 c <- readFile' h ""
                 closeFile h
                 return c
  where
    partial
    readFile' : JFile -> String -> JAVA_IO String
    readFile' h contents =
       do x <- feof h
          if not x then do l <- fread h
                           readFile' h (contents ++ l)
                   else return contents

||| Write the contents of a file into a string
partial -- no error checking!
writeFile : String -> String -> JAVA_IO ()
writeFile fn str = do
  h <- openFile fn Write
  IdrisJava.File.fwrite h str
  closeFile h
