#!/bin/bash

rm -f /tmp/test1.txt

help.sh 'a[$(touch /tmp/test1.txt)] + 42' || (s=$?; echo "test1 got an error"; exit $s)

echo "------"

if [ -f '/tmp/test1.txt' ]; then
    echo "test1 failed"
    exit 1
else
    echo "test1 passed"
    exit 0
fi	

exit 0
