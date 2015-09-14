mvn compile
mvn package
mvn install:install-file -Dfile=`pwd`/target/org.idrisjava-foo.jar -DgroupId=org.idrisjava -DartifactId=foo -Dversion=0.1 -Dpackaging=jar -DgeneratePom=true
