# Plutus Sandbox - *Dockerized* 

## Introduction
If you've ever attempted to get Nix--or any other pain-in-the-neck software--then
you are familiar with the pain and anguish that motivated this project. A quote
from the root README of this project:
> The goal of this project is facilitate fast, easy, and efficient learning and
development of projects on the Plutus platform.

This section of the project aims to complete the **development** piece of that
goal. As it stands at this moment, the contents of this directory accomplish this
in a few ways. To understand them, we will go over exactly what each important 
piece of this directory is doing. Every file is followed by a set of commands
that can be executed to see what exactly it's effect is on the environment.

##### Prerequisites:
1. [Install Docker](https://www.docker.com/)

#### `./Dockerfile`
Defines a Docker image that constructs an ephemeral, very basic Plutus
development environment.
##### Watch it in action:
```bash
# Build a container from the basic image
docker build -t plutus .

# Run that container
docker run -it plutus

# -- Now you are inside nix-shell in the container

# Build the basic project contained in this repository
cabal new-build
```
From there, you can edit the source code and build again to check your changes.  
**Note**: As stated above, this environment is ephemeral, and no changes made
here will persist--a symptom of the beauty of Docker.

#### `./docker-compose.yml`
Defines a docker-compose configuration that uses the Dockerfile described above.
The key feature of this is that it binds the container's source code directory
for the project to the source code in this directory. It also maintains 
Docker-managed volumes that hold the Nix store and cabal directories so startups
and builds are lightning fast even after the container has stopped.
##### Watch it in action:
```bash
# Start the Plutus container as a docker-compose service
docker-compose up --build

# -- This will not attach you to a shell in the container. Instead, we must
# -- find the running container on our system and attach to it ourselves.

### Start a new shell session, or background the docker-compose process

# List all currently running containers
docker ps

# -- The Plutus container ID is a hash that can be found in the far left column
# -- of the list of containers next to the container named docker_plutus or
# -- similar.

# Enter nix-shell inside the Plutus container
docker exec -it <plutus container id> nix-shell

# Build the project
cabal new-build

# Exit the container
exit

# Stop the running Plutus container
docker-compose down

# Or, for a fresh start, you can stop the running container and remove the docker-managed volumes
docker-compose down -v
```
This workflow allows changes to be made to the source code outside of the container, and
the source inside the container will reflect those changes. This is great, and
lends itself well to a quick development feedback cycle where you just use the
container as a build environment while editing your source however you wish
outside the container.

#### `./.devcontainer`
Utilizes Visual Studio Code's fantastic [Container Development](https://code.visualstudio.com/docs/remote/containers)
functionality to make a containerized, configured Visual Studio Code environment
for Plutus development. This is amazing, since we can basically have a full Haskell
IDE inside docker with all centralized configuration.

It is built from an extended docker-compose configuration based on the one above,
so code changes and dependency directories are persisted from inside the container.

##### Watch it in action:
1. Install [Visual Studio Code](https://code.visualstudio.com/)
2. Install the [Remote Development Extension for Visual Studio Code](https://github.com/Microsoft/vscode-remote-release)
3. Open this folder in VSCode.
4. Click the "Open a remote window" in the bottom left corner of the VSCode window.  
![Open a remote window button](https://i.imgur.com/lo5WsLE.jpg)
5. Select the "Remote-Containers: Open Folder in Container..." option from the drop-down list.
![Open Folder in Container option](https://i.imgur.com/eIcKfH3.jpg)

The Dev Container will then launch in a new window, and you can watch it startup
as it did in our previous containers. You can close/exit the window whenever you want
and be assured that your changes will save, and your environment's state will persist.

This is an extensively featured and quite ideal development environment for Plutus.

### Questions and issues
Contact Finley, fmcilwai@uwyo.edu. Or make an issue on this repository with the
`dev-env` label.
