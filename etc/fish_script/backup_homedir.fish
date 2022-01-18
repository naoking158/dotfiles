
function backup_homedir
    echo \n"+ ---------------------------------------------------------------- +"
    echo "+ -------- Backup procedure is started!!!"
    echo "+ ---------------------------------------------------------------- +"\n

    if test -n "$IS_MAC"
        set basedir /Volumes/NSSD/backup/Mac
    else if test -n "$IS_MANJARO"
        set basedir /Volumes/NSSD/backup/Manjaro
    else
        set basedir /Volumes/NSSD/backup/$OS
    end

    set backup_dirs "$HOME/Downloads" "$HOME/src/github.com/naoking158" "$HOME/.dotfiles" "$HOME/drive"

    echo "+ -------- These dirs will be backup:"
    echo \n"$backup_dirs"\n
    read -p '
        echo "  - Press ENTER to confirm the location"
        echo "  - Or specify differents location below"
        echo "    (e.g., $HOME/foo $HOME/bar)"\n
        echo "Backup dirs: "
        ' -S backup_location
    if test -n "$backup_location"
        set backup_dirs $backup_location
    end

    echo \n"+ -------- Backup destination is: "
    echo \n"$basedir"\n
    read -p '
        echo "  - Press ENTER to confirm the location"
        echo "  - Or specify differents location below"
        echo "    (e.g., /Volumes/otherHDD/otherDir)"\n
        echo "Backup into: "
        ' -S input_location
    if test -n "$input_location"
        set basedir $input_location
    end
    
    set path_to_backupdir $basedir/backup-(date +%Y%m%d-%H%M%S)
    if test -e "$basedir"
        set latestbackup (find $basedir -maxdepth 2 -type d -name 'backup-*' | sort | tail -n 1)
    end

    if test -n "$latestbackup"
        echo \n"+ -------- Latest backup is found in $latestbackup"
        echo \n"Execute incremental backup to: "
    else
        echo \n"+ -------- No latest backup is in $basedir"
        echo \n"Execute full backup to: "
    end
    echo \n"$path_to_backupdir"

    echo \n"+ ---------------------------------------------------------------- +"
    echo "+ -------- Final confirmation"
    echo "+ ---------------------------------------------------------------- +"\n
    read -p '
    echo " [yes]  : Proceed"
    echo " [no]   : Cancel"
    echo " [test] : Dry run"
    echo \n"Choose one: "
    ' -S confirm
    
    switch $confirm
        case 'yes'
            mkdir -p $path_to_backupdir
            rsync -avh --exclude {'Pictures','Media'} --link-dest=$latestbackup $backup_dirs $path_to_backupdir
        case 'test'
            rsync -avh --dry-run --stats --exclude {'Pictures','Media'} --link-dest=$latestbackup $backup_dirs $path_to_backupdir
        case '' 'no'
            command echo \n"Backup is canceled."
    end
end
