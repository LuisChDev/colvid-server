#!/usr/bin/env bash

set -exu

result=./result-docker

docker load < $result
docker tag $HEROKU_APP registry.heroku.com/$HEROKU_APP/web
docker push registry.heroku.com/$HEROKU_APP/web

heroku container:release web
