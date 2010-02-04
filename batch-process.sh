#!/bin/sh

cd /data/macho

for x in root/archives/*; do
    b=$(basename $x)
    echo $b
    if [ root/archives/$b/mbox -nt root/archives/$b/index ]; then
        ./macho update $b
    fi
done
