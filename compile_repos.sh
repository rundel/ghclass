#!/bin/sh

if [ $# != 1 ]; then
    echo "Usage: compile_repos.sh <repo suffix>"
    exit 1
fi

teams=('Team1' 'Team2' 'Team3' 'Team4' 'Team5' 'Team6' 'Team7' 'Team8' 'Team9' 'Team10')

suffix=${1}

pwd

for team in ${teams[*]}
do
    folder="${team}_${suffix}"

    cd "${folder}"
    Rscript -e "library(rmarkdown);render('${suffix}.Rmd')"
    cd ..
done