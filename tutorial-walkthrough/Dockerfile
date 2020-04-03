# Base image
FROM nixos/nix

# Use nixpkgs-19.09-darwin to pin GHC to 8.6.5
RUN nix-channel --remove nixpkgs && nix-channel --add https://nixos.org/channels/nixos-19.09 nixpkgs && nix-channel --update

# Configure and install GHC 8.6.5, cabal, openssl, and other required system libraries
RUN nix-env -i ghc-8.6.5 cabal-install wget binutils git && nix-env -iA nixpkgs.openssl.dev

# Help cabal see the extra library and include directories
RUN cabal user-config update -a "extra-lib-dirs: /nix/store/z9vsvmll45kjdf7j9h0vlxjjya6yxgc0-openssl-1.1.1d/lib" \
    && cabal user-config update -a "extra-include-dirs: /nix/store/2v45sw2109m2c7xzld4gc8g5bvjzybc4-openssl-1.1.1d-dev/include"
