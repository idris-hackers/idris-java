
module Main

import IdrisJava

%include java "org.foo.*"
%lib java "org.idrisjava:foo:0.1"

footype : Type
footype = refty "" "Foo"

bartype : Type
bartype = refty "" "Bar"

newfoo : JAVA_IO footype
newfoo = new (JAVA_IO footype)

newbar : String -> Int -> JAVA_IO bartype
newbar s i = new (String -> Int -> JAVA_IO bartype) s i

callfoo : footype -> String -> JAVA_IO ()
callfoo foo s = invokedyn "foodynamic" (footype -> String -> JAVA_IO ()) foo s

main : JAVA_IO ()
main = do
  p <- newfoo
  b <- newbar "Foo" 10
  callfoo p "Wow"
  invokedyn "dodynamic" (bartype -> JAVA_IO ()) b
  putStrLn "End"
