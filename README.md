Idris Java Backend
------------------

This is an updated version of the Java backend for Idris.

Requirements
------------

You need an update to date version of Idris, at least 0.9.19 but I
would really get it from clone from the
[dev branch](https://github.com/idris-lang/Idris-dev).

Its been tested with an updated version of the [Idris Java
runtime](https://github.com/bgaster/idris-java-rts.git) and Idris
development version 0.9.19.

One thing to note is that as the Maven Repository has a version of the
Idris Java Runtime that is compatable with 0.9.14, to use this version
of the frontend you will need to install a built version into a local
repository. This can be done with the following commands:


    git clone https://github.com/bgaster/idris-java-rts.git
    cd idris-java-rts
    mvn compile
    mvn package
    mvn install:install-file -Dfile=${project.basedir}/target/org.idris-lang-idris.jar
	                         -DgroupId=org.idris-lang
	                         -DartifactId=idris
							 -Dversion=0.9.19
							 -Dpackaging=jar
							 -DgeneratePom=true


Then you can build the code generator in a sandbox with Idris dev.

Currently there is an absoutely bare bones Java specfic runtime, which
can be installed as an Idris package. This can be found in the
directory libs. To install the package, simply run the following
commands from the lib directory:

    idris --build java.ipkg
    idris --install java.ipkg

Drop me a line if you have any questions bendict.gaster at uwe.ac.uk.

Usage
------

In general there is no support for the default C IO monad and so while
it will work if you only call putStrLn, for example, as it will not
work in the general case. Instead you should import the `JAVA_IO`
monad found in the module `JavaIO`, in the package `java`. To compile
with the Java backend, using the `java` package use the command line:

    idris --codegen java -p java file.idr -o a.out

where *file.idr* is the Idris file to compile and *a.out* is the
resulting *executable* program.

Here is the standard hello world example (*hello.idr*), but for fun using a foreign call out to
Java to handle the printing.

```haskell
module Main

import JavaIO

systemoutprintln : String -> JAVA_IO ()
systemoutprintln s
   = foreign FFI_Java "System.out.println" (String -> JAVA_IO ()) s

main : JAVA_IO ()
main = systemoutprintln "Hello, World!"
```

which when compiled with:

    idris --codegen java -p java hello.idr -o a.out

produces the output:

    Hello, World!

Current status
--------------

The update is in progress and so far I have tested it with basic
programs and the most resent updates support calling Java from Idris
code, using a JAVA_IO instance of IO'.

Currently there is no support for allocating Java classes, on the
list, and it is also not possible (currently) to call Idris code from
Java.


History
-------

The original work on this backend happened within the Idris compiler
tree, but at some point (2014) Edwin Brady forked it out into [Idris
Hackers Git repo](https://github.com/idris-hackers), where it seems to
have gone rotten. I picked it up to compare against some other work I'm doing
with the Idris compiler and I simply needed a working version!

Here are Edwin's original commit comments:

"This is extracted from the Java backend in Idris-dev. It is untested!
Please feel free to test, maintain, etc :)."
