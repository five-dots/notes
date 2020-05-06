#!/bin/sh

read -p "Please input 1 or 2:" i
if [ $i -eq 1 ]; then
    echo "1 desu"
elif [ $i -eq 2 ]; then
    echo "2 desu"
else
    echo "error!" 1>&2
    exit 1 # エラーで終了させる
fi
