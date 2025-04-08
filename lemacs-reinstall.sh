#!/bin/sh
clear
echo ">>> (re)Installing LEmacs ..."
sleep 2

echo ">>> Deleting packages, grammars and native compilation cache ..."
rm -rf eln-cache* elpa/ tree-sitter/ lemacs-init.elc lemacs-init.el straight/
sleep 2

echo ">>> Starting Emacs and auto-package fetching/installing ..."
sleep 2

# NOTE: for debugging porpouses
# mkdir elpa/
# echo "((emacs-lisp-mode . ((no-byte-compile . t))))" > elpa/.dir-locals.el
#
# emacs --init-dir="./" -nw --debug-init

emacs --init-dir="./" -nw --eval="(lemacs/first-install)" --debug-init
