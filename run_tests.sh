#!/usr/bin/bash
emacs -batch -f package-initialize -L . --eval="(require 'reaction-shell)" -f buttercup-run-discover
