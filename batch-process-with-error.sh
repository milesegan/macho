#!/bin/sh

set -e

cd /data/macho

for x in root/archives/*; do
    b=$(basename $x)
    echo $b
    ./macho update $b
done
