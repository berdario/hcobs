#!/bin/bash
set -euo pipefail

cabal check
cabal sdist
tarball=$(ls dist/*.tar.gz)
curl -X POST https://hackage.haskell.org/packages/ -H "Authorization: X-ApiKey $APIKEY" -H "Accept: text/plain" --form "package=@$tarball" -i || true
