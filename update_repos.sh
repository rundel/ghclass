#!/bin/sh

for repo in */ ; do
    echo "${repo}"
    cd ${repo}
    git pull
    cd ..
    echo "\n"
done