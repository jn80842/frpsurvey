#!/bin/bash

  for ((i = 0; i < 45; i++))
  do
#      mkdir ${i}
#      mv ${i}.txt ${i}/program.txt
       echo ${i}
       cat ${i}/* | wc -l
  done
