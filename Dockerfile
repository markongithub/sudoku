FROM haskell:8

RUN cabal update
RUN cabal install --lib base64-bytestring
RUN cabal install --lib aeson

