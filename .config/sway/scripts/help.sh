# [[file:../config.d/sway_config.org::*Help script][Help script:2]]
#!/usr/bin/env bash
set -x 
# toggles the help wrapper state

STATE_FILE=$HOME/.config/nwg-wrapper/help.state
PID_FILE=$HOME/.config/nwg-wrapper/help.pid

PID=$(cat $PID_FILE 2>/dev/null) 
STATE=$(cat $STATE_FILE 2>/dev/null)

if  [[ $STATE == 'true' && "$1" != "--restore" ]] || [[ "$1" == "--restore" && $STATE == 'false' ]]
then
    if kill -0 $PID; then
        kill -9 $PID
        rm -rf $PID_FILE
    fi
    echo "false" > $STATE_FILE
else
    if ! kill -0 $PID; then
        nwg-wrapper -s help.sh -p left -a end &
        echo $! > $PID_FILE
    fi
    echo "true" > $STATE_FILE
fi
# Help script:2 ends here
