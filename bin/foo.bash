#!/bin/bash

set -f

function hoge() {
    echo "all" $@
    echo $(echo $@ | sed 's/\~/$HOME/')
}

hoge $@
