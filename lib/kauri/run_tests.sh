#!/usr/bin/bash
emacs -batch -f package-initialize -L . --eval="(require 'kauri)" -f buttercup-run-discover
