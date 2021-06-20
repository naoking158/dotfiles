#!/bin/bash

echo ""
echo "==> Check fish shell..."
cat /etc/shells | grep fish
if [ $? = 1 ]; then
    echo "    fish not exists."
    echo "    Please install fish or add path to config file."
    echo "    Exit fisher installer."
    exit
fi

echo ""
echo "==> Fisher install..."
exec fish
curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
fisher install 0rax/fish-bd jethrokuan/z oh-my-fish/theme-clearance
