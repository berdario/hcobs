#!/bin/bash
set -euo pipefail

cabal check
cabal sdist --builddir=dist
tarball=$(ls dist/sdist/*.tar.gz)
curl -X POST https://hackage.haskell.org/packages/ -H "Authorization: X-ApiKey $APIKEY" -H "Accept: text/plain" --form "package=@$tarball" -i || true
