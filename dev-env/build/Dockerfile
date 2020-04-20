FROM nixos/nix AS builder

# Update nix-channel, install git and cachix, and use the IOHK binary cache
RUN nix-channel \
        --remove nixpkgs && nix-channel \
        --add https://nixos.org/channels/nixos-20.03 nixpkgs \
    && nix-channel --update
RUN nix-env -i git \
    && nix-env -iA cachix -f https://cachix.org/api/v1/install && cachix use iohk

# Clone plutus repo
RUN git clone https://github.com/input-output-hk/plutus.git
WORKDIR /plutus

# Build plutus development docker image
RUN nix build -f default.nix docker.development \
    && cp result image.tar.gz

# Container image to copy the dev image from
FROM alpine
COPY --from=builder /plutus/image.tar.gz /docker/image.tar.gz

CMD sleep 30s