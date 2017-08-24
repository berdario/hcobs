# hcobs

To build with Nix, Travis CI is using nixpkgs-unstable (which provides Hedgehog 0.5), to use nixpkgs-unstable locally as well, you might want to run:

    nix-build --arg nixpkgs 'import (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz#1") {}'
