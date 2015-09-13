{- 
  Port of the FFI_C/IO functions found in Idris' Prelude.Interactive to
  FFI_Java/JAVA_IO.
-}
module IdrisJava.Interactive

import IdrisJava

||| Write a single character to stdout
putChar : Char -> JAVA_IO ()
putChar c = invoke "putchar" (Int -> JAVA_IO ()) (cast c)

||| Write a singel character to stdout, with a trailing newline
putCharLn : Char -> JAVA_IO ()
putCharLn c = putStrLn (singleton c)

||| Read a single character from stdin
getChar : JAVA_IO Char
getChar = map cast $ invoke "getchar" (JAVA_IO Int)

||| Get the command-line arguments that the program was called with.
partial
getArgs : JAVA_IO (List String)
getArgs = do n <- numArgs
             ga' [] 0 n
  where
    numArgs : JAVA_IO Int
    numArgs = invoke "idris_numArgs" (JAVA_IO Int)

    getArg : Int -> JAVA_IO String
    getArg x = invoke "idris_getArg" (Int -> JAVA_IO String) x

    partial
    ga' : List String -> Int -> Int -> JAVA_IO (List String)
    ga' acc i n = if (i == n) then (return $ reverse acc) else
                    do arg <- getArg i
                       ga' (arg :: acc) (i+1) n 

-- TODO: the functions replWith and repl in Prelude.Interactive are
-- currently in IO, but I don't see why they can't be IO' ffi () and thus
-- work across any implementation of IO'? Maybe we need a class for getLine
-- support and so on, thus not requiring support for all IO' instances?
