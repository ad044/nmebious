#!/bin/bash

if [[ -z "${DOCKER_RUNNING}" ]]; then
    printf "This script is supposed to be run via Docker.\nIf you'd like to copy the default nginx configuration to your system manually, take a look at nginx.conf.template inside the nginx directory.\n" 1>&2
    exit 64
else
    envsubst '${MAX_FILE_SIZE}' < /etc/nginx/conf.d/default.conf.template > /etc/nginx/conf.d/default.conf
fi


exec "$@"