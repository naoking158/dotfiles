#!/bin/bash

# Ctrl-C
trap 'echo -e "Interrupted.\033]1337;SetColors=bg=000\a"; exit;' INT


# [タブ色を変える]
tab-color() {
  echo -ne "\033]6;1;bg;red;brightness;$1\a"
  echo -ne "\033]6;1;bg;green;brightness;$2\a"
  echo -ne "\033]6;1;bg;blue;brightness;$3\a"
}
tab-color 90 0 0


# set profile
# echo -ne "\033]1337;SetProfile=SSH\a"
echo -ne "\033]1337;SetColors=bg=210\a"
 
# ssh login
/usr/bin/ssh "$@"
 
# set profile(default)
echo -ne "\033]1337;SetProfile=Default\a"
