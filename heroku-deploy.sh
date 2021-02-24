#!/usr/bin/env bash

set -exu

app_name=$1  # cryptic-plateau-71967
result=$(nix-build --no-out-link docker.nix)

docker load < $result
docker tag $app_name registry.heroku.com/$app_name/web
docker push registry.heroku.com/$app_name/web

heroku container:release -a $app_name web
