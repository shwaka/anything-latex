#!/bin/bash
# usage: bash download-arxiv.sh 1701.xxxxx

DOWNLOAD_DIRECTORY="$HOME/Downloads/arxiv_sources"
ARTICLEID=$1

mkdir -p $DOWNLOAD_DIRECTORY/$ARTICLEID
cd $DOWNLOAD_DIRECTORY/$ARTICLEID

wget --user-agent="hoge" http://arxiv.org/abs/$ARTICLEID
wget -O - --user-agent="hoge" http://arxiv.org/e-print/$ARTICLEID | tar zxvf -
