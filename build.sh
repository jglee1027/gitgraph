#!/usr/bin/env bash

buildapp --output gitgraph \
         --asdf-path . \
         --asdf-tree ~/quicklisp/dists \
         --load-system gitgraph \
         --eval '(defun main (args) (declare (ignore args)) (gitgraph:gitgraph))' \
         --entry main
