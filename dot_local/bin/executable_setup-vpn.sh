#!/usr/bin/env bash


set -Ceu
set -x


case $1 in
    "up")
        systemctl start softethervpn-client
        vpncmd localhost /CLIENT /CMD AccountConnect sakamoto
        echo "Wait for 5 seconds..."
        sleep 5
        dhcpcd vpn_vpn-fp
        ip route add 10.0.0.0/8 via 10.224.192.1 dev vpn_vpn-fp proto static metric 200 onlink
        ip route add 20.48.61.83 via 10.224.192.1 dev vpn_vpn-fp proto static metric 200 onlink
        ip route del default via 10.224.192.1 dev vpn_vpn-fp proto dhcp src 10.224.192.11
        ;;
    "down")
        vpncmd localhost /CLIENT /CMD Accountdisconnect sakamoto
        ip route del 10.0.0.0/8 via 10.224.192.1 dev vpn_vpn-fp proto static metric 200 onlink
        ip route del 20.48.61.83 via 10.224.192.1 dev vpn_vpn-fp proto static metric 200 onlink
        nmcli connect down vpn_vpn-fp
        systemctl stop softethervpn-client
        systemctl restart NetworkManager
        ;;
    *)
        echo "An argument must be 'up' or 'down'."
        exit 1
        ;;
esac
