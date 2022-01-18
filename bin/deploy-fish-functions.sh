#!/bin/sh

curr_dir=`pwd`
funcs=`ls ../etc/fish_script/`

for f in $funcs; do
    ln -s "$curr_dir/fish/$f" "$HOME/.config/fish/functions/$f"
done
