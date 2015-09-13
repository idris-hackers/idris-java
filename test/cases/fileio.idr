module Main

import IdrisJava
import IdrisJava.Interactive
import IdrisJava.File

main : JAVA_IO ()
main = do
  h <- openFile "foo.txt" ReadWrite
  b <- validFile h
  fflush h
  if b then putStrLn "valid" else putStrLn "notvalid"
  closeFile h
  
  s <- readFile "foo.txt"
  putStrLn ("contents : " ++ s)
  
  writeFile "bar.txt" "Hello there"
  
  putStr "Enter a character: "
  c <- fgetc stdin
  putCharLn c
  p <- popen "ls" Read
  a <- fgetc p
  putCharLn a
  pclose p
