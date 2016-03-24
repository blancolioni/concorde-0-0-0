#!/bin/bash
rm ai-tests/*
for i in {0..9}
do for j in {0..9}
   do for k in {0..9}
      do ./build/bin/concorde-driver --console --update-count=200 --randomise --system-count=100 --empire-count=10 > ai-tests/test-$i$j$k.txt
      done
   done
done
for name in Byzantium Mallorea Argan ITC Caledonia Macedonia Rome Rovac Mordor Gondor
do echo $name average systems `grep $name ai-tests/test-*.txt | awk '{ total += $2 } END { print total/NR }'`
done
