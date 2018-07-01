#!/bin/bash

for ((i = 0; i < 51; i++))
  do
    racket int-list/${i}/benchmark_intlist.rkt
  done

for ((i = 0; i < 51; i++))
  do
    racket list-list/${i}/benchmark_listlist.rkt
  done
