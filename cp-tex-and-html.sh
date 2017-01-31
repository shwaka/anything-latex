#!/bin/bash

FROM_DIRECTORY="$HOME/Downloads/arxiv_sources"
TO_DIRECTORY="$HOME/Dropbox/programs/tex/arxiv_sources"

mkdir -p $TO_DIRECTORY
cd $FROM_DIRECTORY

for d in *; do
    mkdir -p $TO_DIRECTORY/$d
    cp $d/$d $d/*.tex $TO_DIRECTORY/$d
done
