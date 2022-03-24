#!/usr/bin/env bash


set -Ceu

path_to_elisp="${HOME}/.dotfiles/.config/sway/scripts/edit-with-emacs.el"
emacs -q -l $path_to_elisp
