module IdrisJava.System

import IdrisJava

%include java "java.lang.System"

%default partial
%access public

-- TODO: @bgaster add missing functionality !

||| Quit with a particular exit code
exit : Int -> JAVA_IO ()
exit code = invoke "exit" (Int -> JAVA_IO ()) code

||| Get time in seconds
time : JAVA_IO Bits64
time = do 
  t <- invoke "currentTimeMillis" (JAVA_IO Bits64)
  return (t `div` 1000)       

