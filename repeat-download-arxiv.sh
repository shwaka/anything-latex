#!/bin/bash

for i in $(seq -f %05g 101 500); do
    echo "downloading 1701.$i..."
    bash download-arxiv.sh 1701.$i > /dev/null 2>&1 &
    sleep 5s
done
