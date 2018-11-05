#!/bin/bash

  for ((i = 0; i < 41; i++))
  do
#      mkdir ${i}
#      mv ${i}.txt ${i}/program.txt
       echo ${i}
       cat ${i}/* | wc -l
  done
