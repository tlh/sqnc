#/bin/sh

sbcl --eval '(require :sqnc)' --eval '(sqnc:sqnc)' 2> sqnc-errors.txt
