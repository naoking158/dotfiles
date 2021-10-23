
function hostname_of
    set host $argv[1]
    command ssh -G $host | awk '/^hostname / { print $2 }'
end

function port_of
    set host $argv[1]
    command ssh -G $host | awk '/^port / { print $2 }'
end

# Create an ssh tunnel (args: machineID, remotePORT, hostPORT)
function sshtunnel
    if count $argv = 3
      set host (hostname_of $argv[1])
      set port $argv[2]
      set hostport $argv[3]

        eval (command ssh -M -S mdlssh-socket -fNL $hostport:$host:$port $MDL -l $USER)
        echo "Tunnel from localhost:$hostport to $hostname:$port has been created."
        echo "Be sure to kill the tunnel after you finish your job, by sshexit $hostport"
    else
      echo "Usage: sshtunnel HOST HOSTPORT LOCALPORT"
    end
end

function sshexit
    set hostport $argv[1]

    if ps aux | grep "mdlssh-socket" | grep $hostport -c
        ssh -S mdlssh-socket -O exit $MDL
        echo "All SSH tunnels closed"
    else
        echo "No running tunnels"
    end
end

function rsyncto
    argparse -n mycmdname -x 'i,e' \
        'i/include=+' 'e/exclude=+' -- $argv
    or return

    if set -lq _flag_i
        set myopts '--exclude=*'
        for key in $_flag_i
            set myopts '--include='$key $myopts
        end
    else if set -lq _flag_e
        for key in $_flag_e
            set myopts '--exclude='$key $myopts
        end
    else
        set myopts '--include=*'
    end

    set host (hostname_of $argv[1])
    set port (port_of $argv[1])
    set localpath $argv[2..-2]
    set remotepath (string replace $HOME \~ $argv[-1])

    rsync -avz --copy-unsafe-links -e "ssh -p $port" $localpath $USER@$host:$remotepath --include="*/" $myopts
end

function rsyncfrom
    argparse -n mycmdname o/only= e/exclude= -- $argv
    or return

    if set -lq _flag_i
        set myopts '--exclude=*'
        for key in $_flag_i
            set myopts '--include='$key $myopts
        end
    else if set -lq _flag_e
        for key in $_flag_e
            set myopts '--exclude='$key $myopts
        end
    else
        set myopts '--include=*'
    end

    set host (hostname_of $argv[1])
    set port (port_of $argv[1])
    set remotepath (string replace $HOME \~ $argv[2])
    set localpath $argv[3]

    rsync -avz --copy-unsafe-links -e "ssh -p $port" $USER@$host:$remotepath $localpath --include="*/" $myopts
end

function mdlrsyncto
    set host $argv[1]
    set localpath $argv[2..-2]
    set remotepath (string replace $HOME \~ $argv[-1])
    sshtunnel $host 22 10022
    rsync -avz --progress --bwlimit=5120 -e "ssh -p 10022" $localpath $USER@localhost:$remotepath
    sshexit 10022
end

function mdlrsyncfrom
    set host $argv[1]
    set remotepath (string replace $HOME \~ $argv[2])
    set localpath $argv[3]
    sshtunnel $host 22 10022
    command rsync -avz --progress --bwlimit=5120 -e "ssh -p 10022" $USER@localhost:$remotepath $localpath
    sshexit 10022
end
