#!/bin/sh

echo '(load "poker-elco.lisp")' | sbcl
echo ""
./build.sh
echo "Pokertimer written for elco"
echo "elco is a lisp compiler written in sbcl"
./elco
