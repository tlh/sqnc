#/bin/sh

sbcl --eval '(require :sqnc)' --eval '(use-package :sqnc)' 2> sqnc-errors.txt
