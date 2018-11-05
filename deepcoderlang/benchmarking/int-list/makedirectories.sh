#!/bin/bash

  for ((i = 0; i < 39; i++))
  do
      mkdir ${i}
      mv ${i}.txt ${i}/program.txt
  done
