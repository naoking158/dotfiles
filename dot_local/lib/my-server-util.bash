#!/usr/bin/env bash


function hostname_of() {
    ssh -G $1 | awk '/^hostname / { print $2 }'
}

function username_of() {
    ssh -G $1 | awk '/^user / { print $2 }'
}

function port_of() {
    ssh -G $1 | awk '/^port / { print $2 }'
}

function sshtunnel() {
    if [[ $# -ne 3 ]]; then
        e_error "Usage: sshtunnel HOST HOSTPORT LOCALPORT"
        exit 1
    else
        local host=$(hostname_of $1)
        local user=$(username_of $1)
        local port=$2
        local hostport=$3

        ssh -M -S mdlssh-socket -fNL ${hostport}:${host}:${port} ${MDL} -l ${user}
        echo "Tunnel from localhost:$hostport to $host:$port has been created."
        echo "Be sure to kill the tunnel after you finish your job, by sshexit $hostport"
    fi
}

# function sshtunnel() {
#     if [[ $# -ne 3 ]]; then
#         echo "Usage: sshtunnel HOST HOSTPORT LOCALPORT"
#     else
#         local host=$(hostname_of $1)
#         local port=$2
#         local hostport=$3

#         ssh -M -S mdlssh-socket -fNL ${hostport}:${host}:${port} ${MDL} -l ${USER}
#         echo "Tunnel from localhost:$hostport to $hostname:$port has been created."
#         echo "Be sure to kill the tunnel after you finish your job, by sshexit $hostport"
#     fi
# }

function sshexit() {
    local hostport=$1

    if [[ $(ps aux | grep "mdlssh-socket" | grep ${hostport} -c) ]]; then
        ssh -S mdlssh-socket -O exit $MDL
        echo "All SSH tunnels closed"
    else
        echo "No running tunnels"
    fi
}

function get_nth () {
  local n=$1
  shift
  eval echo \$${n}
}

# function rsyncfrom
#     argparse -n mycmdname o/only= e/exclude= -- $argv
#     or return

#     if set -lq _flag_i
#         set myopts '--exclude=*'
#         for key in $_flag_i
#             set myopts '--include='$key $myopts
#         end
#     else if set -lq _flag_e
#         for key in $_flag_e
#             set myopts '--exclude='$key $myopts
#         end
#     else
#         set myopts '--include=*'
#     end

#     set host (hostname_of $argv[1])
#     set port (port_of $argv[1])
#     set remotepath (string replace $HOME \~ $argv[2])
#     set localpath $argv[3]

#     rsync -avz --copy-unsafe-links -e "ssh -p $port" $USER@$host:$remotepath $localpath --include="*/" $myopts
# end

# function mdlrsyncto
#     set host $argv[1]
#     set localpath $argv[2..-2]
#     set remotepath (string replace $HOME \~ $argv[-1])
#     sshtunnel $host 22 10022
#     rsync -avz --progress --bwlimit=5120 -e "ssh -p 10022" $localpath $USER@localhost:$remotepath
#     sshexit 10022
# end

# function mdlrsyncfrom
#     set host $argv[1]
#     set remotepath (string replace $HOME \~ $argv[2])
#     set localpath $argv[3]
#     sshtunnel $host 22 10022
#     command rsync -avz --progress --bwlimit=5120 -e "ssh -p 10022" $USER@localhost:$remotepath $localpath
#     sshexit 10022
# end
