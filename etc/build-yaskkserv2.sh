#!/usr/bin/env bash


set -Ceu

declare -r user_local_exec_path="${HOME}/.local/bin"
declare -r server_program="${user_local_exec_path}/yaskkserv2"
declare -r dictionary_program="${user_local_exec_path}/yaskkserv2_make_dictionary"
declare -r base_path="/tmp/yaskkserv2"

function has() {
    type "$1" >/dev/null 2>&1
    return $?
}

if has cargo && ! has $server_program && ! has $dictionary_program; then
    [[ -e $base_path ]] || git clone https://github.com/wachikun/yaskkserv2 $base_path &&
    cd $base_path &&
    cargo build --release --manifest-path "${base_path}/Cargo.toml" &&
    [[ -e $server_program ]] || cp -r "${base_path}/target/release/yaskkserv2" $server_program &&
    [[ -e $dictionary_program ]] || cp -r "${base_path}/target/release/yaskkserv2_make_dictionary" $dictionary_program &&
    exit 0 || exit 1                
else
    echo "You have." 
fi
