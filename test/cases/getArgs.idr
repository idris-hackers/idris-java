module Main

import IdrisJava
import IdrisJava.Interactive

main : JAVA_IO ()
main = do
  [a1 , a2] <- getArgs
  putStrLn (a1 ++ a2)
  putChar 'z'
  putCharLn 'a'
  --c <- getChar
  --putCharLn c
  putStr "Please enter name: "
  l <- getLine
  putStrLn ("Hello " ++ l)
