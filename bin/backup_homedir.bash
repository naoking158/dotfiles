#!/usr/bin/env bash
#
#
# ===============
# This is a modified rm program to move SRC to RM_ALT_TRASH instead of remove it.
#
# Usage:
#     $0 [OPTION]... SRC [SRC]...
#
# Options:
#     -h, --help
#     -n, --dry-run
#     -d, --delete  ----  `rm -rf SRC` is executed using `/bin/rm` (Be careful!!!)
#         --restore ----  Restore previously moved files to their original locations.
#                             Previously moved history is saved in RM_ALT_HIST
#
# Default values:
#     RM_ALT_TRASH = ~/.myTrash
#     RM_ALT_HIST  = $RM_ALT_TRASH/.moved_hist
#


set -Ceu
set -o functrace

function is_macos() { [[ $(uname -s) =~ Darwin ]]; }
function is_manjaro() { [[ $(uname -r) =~ .*MANJARO ]]; }

declare EXCLUDES=("Library" "Applications" "Documents" "Pictures" "Media" "Movies")

# Avoid split file name with white space
SAVEIFS=IFS
IFS=$(echo -en "\n\b")

function postprocess() {
    export IFS=$SAVEIFS
}
trap postprocess EXIT

function failure() {
    local lineno=$1
    local msg=$2
    echo "Failed at $lineno: $msg"
}
trap 'failure ${LINENO} "$BASH_COMMAND"' ERR

function help () {
    awk -v CMD="$(basename $0)" 'NR > 2 {
    if (/^#/) {
        sub("^# ?", "");
        sub("\\$0", CMD);
        sub("Be careful!!!", "\033[31;1m\&\033[0m");
        print }
    else { exit }
    }' $0
    exit 1
}

function e_newline() {
    printf "\n"
}

function e_header() {
    printf " \033[37;1m%s\033[m\n" "$*"
}

function e_important() {
    printf " \033[31;1m%s\033[m\n" "$*"
}

function ink() {
    if [ "$#" -eq 0 -o "$#" -gt 2 ]; then
        echo "Usage: ink <color> <text>"
        echo "Colors:"
        echo "  black, white, red, green, yellow, blue, purple, cyan, gray"
        return 1
    fi

    local open="\033["
    local close="${open}0m"
    local black="0;30m"
    local red="1;31m"
    local green="1;32m"
    local yellow="1;33m"
    local blue="1;34m"
    local purple="1;35m"
    local cyan="1;36m"
    local gray="0;37m"
    local white="$close"

    local text="$1"
    local color="$close"

    if [ "$#" -eq 2 ]; then
        text="$2"
        case "$1" in
            black | red | green | yellow | blue | purple | cyan | gray | white)
            eval color="\$$1"
            ;;
        esac
    fi

    printf "${open}${color}${text}${close}"
}

function e_error() {
    printf " \033[31m%s\033[m\n" "✖ $*" 1>&2
}

function die() {
    e_error "$1" 1>&2
    exit "${2:-1}"
}

function echo_start() {
    e_newline
    e_header "Backup directories in $HOME"
    e_newline
}

function backup_homedir() {
    echo_start

    
}


backup_homedir
