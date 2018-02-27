#!/bin/sh

set -ue

docker load < result
docker tag matrix-bot:latest plapadoo/matrix-bot:latest
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
docker push plapadoo/matrix-bot:latest
