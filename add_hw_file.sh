#!/bin/sh

if [ $# -lt 3 ]; then
    echo "Usage: add_hw_file.sh <repo suffix> <message> <file1> <file2> ..."
    exit 1
fi

github='git@github.com:Sta523-Fa16/'
teams=('Team0' 'Team1' 'Team2' 'Team3' 'Team4' 'Team5' 'Team6' 'Team7' 'Team8' 'Team9' 'Team10')

suffix=${1}
message=${2}
shift 2

for team in ${teams[*]}
do
    folder="${team}${suffix}"
    repo="${github}${folder}"

    git clone ${repo}

    for file in "$@"
    do
        cp ${file} ${folder}/
    done

    cd ${folder}
    git add .
    git commit -m "${message}"
    git push
    cd ..

    rm -rf ${folder}
done