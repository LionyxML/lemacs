#!/bin/sh

clear
echo ">>> (re)Installing LEmacs ..."
sleep 2

echo ">>> Deleting packages, grammars and native compilation cache ..."
rm -rf eln-cache/ elpa/ tree-sitter/
sleep 2

echo ">>> Starting Emacs and auto-package fetching/installing ..."
sleep 2
emacs --init-dir="./" -nw --eval="(lemacs/first-install)"
