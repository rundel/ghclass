#!/bin/sh

if [ $# != 1 ]; then
    echo "Usage: grab_repos.sh <repo suffix>"
    exit 1
fi

github='git@github.com:Sta523-Fa16/'
teams=('Team1' 'Team2' 'Team3' 'Team4' 'Team5' 'Team6' 'Team7' 'Team8' 'Team9' 'Team10')

suffix=${1}

for team in ${teams[*]}
do
    folder="${team}${suffix}"
    repo="${github}${folder}"

    git clone ${repo}
done
