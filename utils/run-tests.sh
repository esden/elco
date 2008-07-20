#!/bin/sh

echo "(require 'asdf) (asdf:operate 'asdf:load-op 'elco-test) (elco-test:run-tests)" | sbcl
