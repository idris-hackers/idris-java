Idris Java Backend
------------------

This is an updated version of the Java backend for Idris.

Requirements
------------

Make sure you have a recent version of GHC installed, this has all
been tested with 7.10.2. If you are new to GHC, then I recomend using
the [Haskell Platform](https://www.haskell.org/platform/).

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


Then you can build the code generator in a sandbox with Idris dev. For instance
if you have a directory *<idris-root>* you can create a toplevel sandbox as follows:

    cabal sandbox init --init .

then clone idris-dev with:

    git clone https://github.com/idris-lang/Idris-dev.git
    cd Idris-dev
	cabal sandbox init --init <idris-root>

then follow the rest of the instructions on the
[Idris wiki](https://github.com/idris-lang/Idris-dev/wiki/Installing-an-Idris-Development-version-in-a-sandbox)
to build the compile. Note: please be sure to to build with FFI enabled.

Add the *<idris-root>/bin* to your path (e.g. `export PATH=...`). Now
your ready to build the Java code generator. Clone and bujild *idris-java* with
the following commands:

    git clone https://github.com/bgaster/idris-java.git
    cd idris-java
	cabal sandbox init --init <idris-root>
	cabal install --only-dependencies
	cabal configure
	cabal install

Assuming all went well, then you will have the executables *idris* and
*idris-java* (along with other backends) in the directory *<idris-root>/bin*.

Currently there is an absoutely bare bones Java specfic runtime, which
can be installed as an Idris package. This can be found in the
directory libs. To install the package, simply run the following
commands from the lib directory:

    idris --build idrisjava.ipkg
    idris --install idrisjava.ipkg

Usage
------

In general, there is no support for the default C IO monad and so while
it will work if you only call `putStrLn`, for example, as it will not
work in the general case. Instead you should import the `JAVA_IO`
monad found in the module `IdrisJava`, in the package `idrisjava`. To compile
with the Java backend, using the `idrisjava` package use the command line:

    idris --codegen java -p idrisjava file.idr -o a.out

where *file.idr* is the Idris file to compile and *a.out* is the
resulting *executable* program.

Here is the standard hello world example (*hello.idr*), but for fun
using a foreign call out to Java to handle the printing.

```haskell
module Main

import IdrisJava

systemoutprintln : String -> JAVA_IO ()
systemoutprintln s
   = javacall "System.out.println" (String -> JAVA_IO ()) s

main : JAVA_IO ()
main = systemoutprintln "Hello, World!"
```

which when compiled with:

    idris --codegen java -p idrisjava hello.idr -o a.out

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

Contact
-------

Drop me a line if you have any questions bendict.gaster at uwe.ac.uk.


## Books

[Type-Driven Development with Idris](https://www.manning.com/books/type-driven-development-with-idris) by Edwin Brady (Manning Publications). [Chapter 1](https://manning-content.s3.amazonaws.com/download/8/99b07b5-ad1d-4272-860b-c323b3f5bf4c/Brady_TDDwithIdris_MEAP_ch1.pdf)
