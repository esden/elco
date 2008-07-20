#!/bin/sh

gcc -g -O3 -Wall elco-driver.c -c
gcc -g -O3 -Wall elco-driver.o elco.s -o elco
