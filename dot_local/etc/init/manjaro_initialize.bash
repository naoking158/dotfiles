#!/bin/bash


set -Ceu

function __hashr() {
    if [ -n "${ZSH_VERSION:+x}" ]; then
        rehash
    elif [ -n "${POSH_VERSION:+x}" ]; then
        :  # pass
    else
        hash -r
    fi
}

sudo pacman-mirros --fasttrack &&
sudo pacman -Syyu &&

# for japanese input
sudo pacman -S fcitx5-im

# install paru
sudo pacman -S yay &&
    __hashr &&
    yay paru
