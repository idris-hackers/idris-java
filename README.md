Idris Java Backend
------------------

This is an updated version of the Java backend for Idris.

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

Drop me a line if you have any questions bendict.gaster at uwe.ac.uk.

Current status
--------------

The update is in progress and so far I have tested it with basic
programs, however, there is currently no support for the updated
foreign function interface (next on the list), but most other stuff should
work.


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
