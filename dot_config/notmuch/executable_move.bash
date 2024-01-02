#!/usr/bin/env bash

set -Ceu

echo "Updating notmuch database"
notmuch new --no-hooks

# https://notmuchmail.org/pipermail/notmuch/2019/028956.html

# Move a message file while removing its UID-part
function safeMove {
    LATTER="${1##*/}";
    LATTER="${LATTER%%,*}";    
    DEST="${2}/${LATTER}";
    mv -f "${1}" "$DEST"
}

SAVEIFS=IFS
IFS=$(echo -en "\n\b")

MAILDIR=$HOME/Maildir
ADDRESS=("bbo-naoki@bbo.cs.tsukuba.ac.jp"
         "private-nok.skmt.snow@gmail.com"
         "work-sakamoto@fixpoint.co.jp")
for i in "${ADDRESS[@]}"; do
    Name=${i%%-*}
    Address=${i##*-}
    
    # Move all deleted messages to the Trash folder
    RES=($(notmuch search --exclude=false --output=files \'tag:del and not folder:Trash\'))
    if [[ -z ${RES+x} ]]; then
        echo "No deleted messages."
    else
        echo Moving "${#RES[@]}" \
             deleted messages to the Trash folder
        
        for i in "${RES[@]}"; do
            [[ ! "${i}" =~ ^.*/\[Gmail\]/Trash.*$ ]] && {
                safeMove "$i" "${MAILDIR}/${Name}/[Gmail]/Trash/cur";
            }
        done
    fi
 
    # # Move all spam messages to the Spam folder
    # echo Moving $(notmuch count --output=files tag:spam AND NOT folder:Spam) \
    #      spam-marked messages to the Spam folder
    # for i in $(notmuch search --output=files tag:spam AND NOT folder:Spam); do
    #     safeMove $i ${MAILDIR}/Spam/cur
    # done

    # Move all archived messages from Inbox to All Mail folder of Gmail
    RES=($(notmuch search --output=files \'tag:archived and folder:\"${Name}/Inbox\"\'))
    if [[ -z ${RES+x} ]]; then
        echo "No archived messages in your inbox."
    else
        echo Moving "${#RES[@]}" \
             archived messages from Inbox to Archive folder
        if [[ ! -d "${MAILDIR}/${Name}/[Gmail]/All Mail/cur" ]]; then
            mkdir -p "${MAILDIR}/${Name}/[Gmail]/All Mail/cur"
        fi
        for i in "${RES[@]}"; do
            [[ ! "${i}" =~ ^.*(Sent|All).*$ ]] && {
                safeMove "$i" "${MAILDIR}/${Name}/[Gmail]/All Mail/cur";
            }
        done
    fi
done

IFS=$SAVEIFS

echo "Syncing with Webmail"
mbsync -a

echo "Updating notmuch database"
notmuch new --no-hooks
