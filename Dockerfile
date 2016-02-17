FROM aaroncr/idris-dev
ADD . idris-java
WORKDIR idris-java
RUN cabal install
WORKDIR libs
RUN idris --install idrisjava.ipkg
WORKDIR ../..


