#!/bin/sh

gcc -m32 -g -O3 -Wall elco-driver.c -c
gcc -m32 -g -O3 -Wall elco-driver.o elco.s -o elco
