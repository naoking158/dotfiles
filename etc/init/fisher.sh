#!/bin/bash

echo ""
echo "==> Check fish shell..."
if ! [ -x "$(command -v fish)" ]; then
    echo "Error: fish shell is not installed." \
         "Please install fish." \
         "Exit fisher installer." >&2
    exit 1
fi

echo ""
echo "==> Fisher install..."
fish -c "curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher; and fisher install 0rax/fish-bd jethrokuan/z IlanCosman/tide@v5"
