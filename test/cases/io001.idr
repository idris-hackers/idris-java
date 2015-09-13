module Main

import IdrisJava

systemoutprintln : String -> JAVA_IO ()
systemoutprintln s
   = invoke "System.out.println" (String -> JAVA_IO ()) s

main : JAVA_IO ()
main = systemoutprintln "Hello, World!"
