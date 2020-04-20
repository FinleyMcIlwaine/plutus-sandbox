#!/bin/bash

# Get the image out of the intermediate builder
docker build -t builder .
docker run --name holder builder &
docker cp holder:/docker/image.tar.gz ./image.tar.gz
docker stop holder

# Load the image
docker load < image.tar.gz
