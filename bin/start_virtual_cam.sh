#!/usr/bin/env bash


set -Ceu

# die returns exit code error and echo error message
function die() {
    e_error "$1" 1>&2
    exit "${2:-1}"
}

function e_error() {
    printf " \033[31m%s\033[m\n" "✖ $*" 1>&2
}

function is_exists() {
    type "$1" >/dev/null 2>&1
    return $?
}

if ! is_exists mtplvcap; then
    die "mtplvcap is not found"
fi

if ! is_exists obs; then
    die "obs is not found"
fi

mtplvcap --max-resolution && obs --startvirtualcam
