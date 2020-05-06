#!/bin/bash

for i in {1..5}; do
    if [ $i -eq 3 ]; then
        echo "skip 3."
        break # continue
    fi
    echo $i
done
