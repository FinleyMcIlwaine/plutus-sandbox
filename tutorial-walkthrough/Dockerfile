# Base image
FROM nixos/nix

# Use nixpkgs-20.03 for GHC 8.8.2
RUN nix-channel --remove nixpkgs && nix-channel --add https://nixos.org/channels/nixos-20.03 nixpkgs && nix-channel --update \
    # Install GHC 8.8.2
    && nix-env -f "<nixpkgs>" -iA 'haskell.compiler.ghc882' \
    # Install cabal, openssl, and other required system libraries
    && nix-env -i cabal-install binutils git && nix-env -iA nixpkgs.openssl.dev \
    # Help cabal see the extra library and include directories
    && cabal user-config update -a "extra-lib-dirs: /nix/store/z9vsvmll45kjdf7j9h0vlxjjya6yxgc0-openssl-1.1.1d/lib" \
    && cabal user-config update -a "extra-include-dirs: /nix/store/2v45sw2109m2c7xzld4gc8g5bvjzybc4-openssl-1.1.1d-dev/include"
