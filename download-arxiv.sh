#!/bin/bash

DOWNLOAD_DIRECTORY="$HOME/Dropbox/programs/tex/arxiv_sources"
ARTICLEID=1701.08305

mkdir -p $DOWNLOAD_DIRECTORY/$ARTICLEID
cd $DOWNLOAD_DIRECTORY/$ARTICLEID

wget -O - --user-agent="hoge" http://arxiv.org/e-print/$ARTICLEID | tar zxvf -
