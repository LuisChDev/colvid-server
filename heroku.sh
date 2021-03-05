#!/usr/bin/env bash

set -exu

app_name=$1  # frozen-reef-80565
result=./result-docker

docker load < $result
docker tag $app_name registry.heroku.com/$app_name/web
docker push registry.heroku.com/$app_name/web

heroku container:release -a $app_name web
