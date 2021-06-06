# -l / --local
#     現在のブロックに対して
#     (強制的に)ローカル変数にする。
#     ローカル変数以外の
#     同名の変数がある場合であっても。
# -g / --global
#     グローバル変数にする。
#     グローバルではない変数は
#     end に対応するブロックが終了するまで隠される。
# -U / --universal
#     ユニバーサル変数 にする。
#     現在のコンピュータで動いている
#     現ユーザの全fishセッションで
#     変数は共有される。
#     シェルを再起動しても保持される。
# -x / --export
#     子プロセスにもexport(伝播)される。
#     つまり「環境変数」となる。
# -u / --unexport
#     子プロセスにexport されない ようにする。



################################################################
# Change the user name
################################################################
set -xg USER naoki    # A local network user

################################################################
# Path to the directory fo 'USER' on the local HD
# Make sure that the directory is created for each local machine
set -xg localhome /Users/$USER

################################################################
set -xg HOST gateway.mdl.cs.tsukuba.ac.jp    # Host Name for GIP
set -xg MDL ssh-user@$HOST

################################################################
# PATH
################################################################
#To solve a locate problem happens in ipython notebook
set -xg LC_ALL en_US.UTF-8
set -xg LANG en_US.UTF-8


if test (uname -s) = "Darwin"
   # libgccjit
   set -xg LIBRARY_PATH /usr/local/opt/libgccjit/lib/gcc/11 $LIBRARY_PATH

    # Homebrew
    set -xg HOMEBREW_EDITOR "/usr/local/bin/emacs -q -nw"

    # ghq root
    set -xg GHQ_ROOT "$HOME/drive"

    set PATH /Library/Tex/texbin $PATH
    set PATH /usr/local/bin $PATH
    set PATH /usr/local/texlive/2020basic/bin/x86_64-darwin $PATH
    set PATH $HOME/.local/bin $PATH
    set PATH /opt/local/bin $PATH

    set PYTHONPATH ~/drive/github.com/akimotolab/NaokiSakamoto/source/script $PYTHONPATH
    set -x USE_DAAL4PY_SKLEARN YES

    # Node.js
    set PATH $HOME/.nodebrew/current/bin $PATH

    # Rust
    set -U fish_user_paths $fish_user_paths $HOME/.cargo/bin

    # fzf
    set -U FZF_LEGACY_KEYBINDINGS 0
    set -U FZF_REVERSE_ISEARCH_OPTS "--reverse --height=100%"
    set -x FZF_DEFAULT_COMMAND 'rg -a --files --hidden --no-ignore --follow'

    set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

    # For compilers to find openblas you may need to set:
    set -gx LDFLAGS "-L/usr/local/opt/openblas/lib"
    set -gx CPPFLAGS "-I/usr/local/opt/openblas/include"

    # For pkg-config to find openblas you may need to set:
    set -gx PKG_CONFIG_PATH "/usr/local/opt/openblas/lib/pkgconfig" $PKG_CONFIG_PATH

    set -gx LDFLAGS "-L/usr/local/opt/libxml2/lib" $LDFLAGS
    set -gx CPPFLAGS "-I/usr/local/opt/libxml2/include" $CPPFLAGS
    set -gx PKG_CONFIG_PATH "/usr/local/opt/libxml2/lib/pkgconfig" $PKG_CONFIG_PATH

    set -gx LDFLAGS "-L/usr/local/opt/imagemagick@6/lib"
    set -gx CPPFLAGS "-I/usr/local/opt/imagemagick@6/include"
    set -gx PKG_CONFIG_PATH "/usr/local/opt/imagemagick@6/lib/pkgconfig"
end


################################################################
# Functions
################################################################
alias gomi='trash -i -r'

# cd -> ls
functions --copy cd standard_cd

function cd
    standard_cd $argv; and ls
end

function du
    command /usr/bin/du -shc * | sort -h
end

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
    set running (ps aux | grep "mdlssh-socket" | grep $hostport -c)

    if ps aux | grep "mdlssh-socket" | grep $hostport -c
        ssh -S mdlssh-socket -O exit $MDL
	    echo "All SSH tunnels closed"
    else
    	echo "No running tunnels"
    end
end


function rsyncto
    argparse -n mycmdname o/only= e/exclude= -- $argv
    or return

    if set -lq _flag_o
        set include $_flag_o
    else
        set include '*'
    end
    if set -lq _flag_e
        set exclude $_flag_e
    else
        set exclude '*'
    end

    set host (hostname_of $argv[1])
    set port (port_of $argv[1])
    set localpath $argv[2..-2]
    set remotepath (string replace $HOME \~ $argv[-1])

    rsync -avz --copy-unsafe-links -e "ssh -p $port" $localpath $USER@$host:$remotepath --include="*/" --include="$include" --exclude="$exclude"
end

function rsyncfrom
    argparse -n mycmdname o/only= e/exclude= -- $argv
    or return

    if set -lq _flag_o
        set include $_flag_o
    else
        set include '*'
    end
    if set -lq _flag_e
        set exclude $_flag_e
    else
        set exclude '*'
    end

    set host (hostname_of $argv[1])
    set port (port_of $argv[1])
    set remotepath (string replace $HOME \~ $argv[2])
    set localpath $argv[3]

    rsync -avz --copy-unsafe-links -e "ssh -p $port" $USER@$host:$remotepath $localpath --include="*/" --include="$include" --exclude="$exclude"
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


function send_and_install_anaconda_on
    if count $argv > /dev/null
	if [ $argv = --help ]
	    printf "
	    Description:
	    Send Anaconda*.sh and license*.txt to server.
	    Next, install Anaconda*.sh, update all and install accelerate.

	    If older version of anaconda (e.g., anaconda2) is already exists on server,
	    install is skipped.

	    Usage:
	    send_and_install_anaconda_on <server name>

	    Examples:
	    # You are in local (e.g., /home/user/)
	    > ls
	    #  Anaconda*.sh  license*.txt
	    > send_and_install_anaconda_on debian1 debian2 sun sirius

	    Option:
	    --help    Print this help and exit.
	    "
	else
	    set -l anaconda_with_wildcard Anaconda*.sh
	    if test -e $anaconda_with_wildcard
		set anaconda (ls $anaconda_with_wildcard)
		for hostname in $argv
		    fish -c "_install_anaconda_on_server $hostname $anaconda"
		end
	    else
		echo "Anaconda~.sh is not exist here."
	    end
	end
    else
	echo "You must input server name."
	echo "If you want know how to use this function, type `send_and_install_anaconda_on --help`."
    end
end


function _install_anaconda_on_server
    set hostname $argv[1]
    set anaconda $argv[2]
    set answers '\nyes\n\nyes\nno\n'
    set -l license_with_wildcard license*.txt
    if test -e $license_with_wildcard
        set -l license (ls $license_with_wildcard)
        ssh $USER@$hostname.local "
        if [ ! -d '.continuum' ]; then
        mkdir .continuum;
        fi;
        exit"
        rsyncto $hostname '\./\.continuum' $license
        echo \n'Done send '$license' for '$hostname'.'\n
    else
	    echo \n'Pass "mkdir .continuum" and "send license", because license.txt not exists.'\n
    end

    rsyncto $hostname "\./" $anaconda
    echo \n'Done send '$anaconda' for '$hostname'.'\n
    nohup ssh $USER@$hostname.local "
    if [ ! -d anaconda* ]; then
    printf '$answers' | bash '$anaconda';
    rm $anaconda;
    source .bashrc;
    echo -e '\nDone install anaconda on $hostname.\n';
else
    echo -e '\nanaconda already installed. Installing anaconda is skipped.\n';
    fi;
    echo -e 'Next step -> `conda update` and `install accelerate`.\n';
    yes | conda update --all;
    yes | conda install accelerate;
    echo -e '\nComplete install and update anaconda on $hostname.\n';
    exit" > 'progress_of_install_anaconda.out' &
end



function clone_alab_repositories_on
    if count $argv > /dev/null
	if [ $argv = --help ]
	    printf "
	    Description:
	    Clone repositories on server.
	    If Optimization/IterativeAlgorithmCore in clone repositories,
	    then execute the following, `pip --no-cache-dir install -I -e .`.

	    Usage:
	    clone_alab_repositories_on <server name>

	    Examples:
	    # You are in local
	    > clone_alab_repositories_on debian1 debian2 sun sirius

	    Option:
	    --help    Print this help and exit.
	    "
	else
	    echo \n"/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/"
	    echo " +++  Set repositories will be cloned.  +++"
	    echo "/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/"
	    echo "These repositories will be cloned:"
	    echo "akimotolab/Optimization, akimotolab/IterativeAlgorithmCore"\n

	    read -p '
	    echo "  - Press ENTER to confirm the repository"
	    echo "  - Or specify differents repositories below"
	    echo "    (e.g., akimotolab/trunk akimotolab/ServerAdmin)"\n
	    echo "Clone repositories: "
	    ' -l input_repositories
	    if [ $input_repositories = '' ]
		set repositories 'akimotolab/Optimization' 'akimotolab/IterativeAlgorithmCore'
		echo $repositories
	    else
		set repositories $input_repositories
	    end

	    echo \n"/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/"
	    echo "+ Set location where the repository will be cloned. +"
	    echo "/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/"
	    echo "The repository will now be cloned into this location:"
	    echo "(remote server)~/repositories/"\n

	    read -p '
	    echo "  - Press ENTER to confirm the location"
	    echo "  - Or specify differents location below"
	    echo "    (e.g., ~/github/akimotolab/)"\n
	    echo "Clone into: "
	    ' -l input_location
	    if [ $input_location = '' ]
		set clone_location '~/repositories/'
		echo $clone_location
	    else
		set clone_location $input_location
	    end

	    read -p 'echo \n"github username: "' -l username
	    read -p 'echo "github passward: "' --silent pw

	    for hostname in $argv
		nohup ssh $USER@$hostname.local "pip install quadprog" &
		for repository in $repositories
		    fish -c "_clone_repository $hostname $clone_location $repository $username $pw"
		end
		echo \n"Done send job for $hostname."\n
	    end
	    # remove password
	    set -e $pw
	end
    else
	echo "You must input server name."
	echo "If you want know how to use this function, type `clone_alab_repositories_on --help`."

    end

end


function _clone_repository --argument-names 'hostname' 'location' 'repository' 'user' 'pw'
    set -l repository_dir_names (string split / $repository)
    set repository_dir_name $repository_dir_names[-1]
    nohup ssh $USER@$hostname.local "
    if [ ! -d $location ];
    then
    mkdir -p $location;
    fi;
    cd $location;

    if [ ! -d $repository_dir_name ];
    then
    git clone 'https://$user:$pw@github.com/$repository.git';

    if [ '$repository_dir_name' == 'Optimization' ] || [ '$repository_dir_name' == 'IterativeAlgorithmCore' ];
    then
    cd $repository_dir_name;
    pip --no-cache-dir install -I -e .;
    fi;
else
    echo -e '\n$repository_dir_name is already exists.\n';
    fi;
    echo -e '\nComplete clone $repository_dir_name on $hostname.\n';
    exit
    " > 'progress_of_clone.out' &
end

################################################################

function fish_on_server
    read -p 'echo "USERNAME: "' user
    read -p 'echo "PASSWORD: "' --silent pw
    for hostname in $argv
	fish -c "_install_fish $hostname $user $pw"
	echo \n"send $hostname."\n
    end
end


function _install_fish --argument-names 'hostname' 'user' 'pw'
    nohup command sshpass -p "$pw" ssh -o StrictHostKeyChecking=no $user@$hostname.local "
    echo 'deb http://download.opensuse.org/repositories/shells:/fish:/release:/2/Debian_8.0/ /' > fish.list;
    echo -e '$pw\n' | sudo -S mv 'fish.list' '/etc/apt/sources.list.d/';
    sudo wget http://download.opensuse.org/repositories/shells:fish:release:2/Debian_8.0/Release.key;
    sudo apt-key add - < Release.key;
    echo -e '$pw\n' | sudo -S -;
    yes | sudo -S apt-get update;
    echo -e '$pw\n' | sudo -S -;
    yes | sudo -S apt-get upgrade;
    echo -e '$pw\n' | sudo -S -;
    yes | sudo -S apt-get install fish;
    echo -e '\n Done install fish on $hostname.\n';
    exit
    " > 'fish_on_server.out' &
end


function backup_homedir
    set basedir /Volumes/NSSD/backup

    echo \n"/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/"
    echo "+ Set location where the HOME dirs will be backup. +"
    echo "/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/"
    echo \n"The HOME dirs will now be backup into this location:"\n
    echo "$basedir"\n
    read -p '
        echo "  - Press ENTER to confirm the location"
        echo "  - Or specify differents location below"
        echo "    (e.g., /Volumes/otherHDD/otherDir)"\n
        echo "Backup into: "
        ' -S input_location

    if test -n $input_location
        set basedir $input_location
    end

    set latestbackup (find $basedir -maxdepth 2 -type d -name 'backup-*' | sort | tail -n 1)
    set path_to_backupdir $basedir/backup-(date +%Y%m%d-%H%M%S)

    if test -z $latestbackup
        echo "No latest backup is in $basedir"\n
        set mes "Conduct full backup? [y/N/test] "
    else
        echo "Latest backup is found in $latestbackup"\n
        set mes "Conduct incremental backup to $path_to_backupdir? [y/N/test] "
    end

    read -P $mes -l confirm

    switch $confirm
        case Y y
            rsync -avh --link-dest=$latestbackup $HOME/ $path_to_backupdir
        case 'test'
            rsync -avh --dry-run --stats --link-dest=$latestbackup $HOME/ $path_to_backupdir
        case '' N n
            command echo \n"Backup is canceled."
    end
end

################################################################

function fish_user_key_bindings
    bind \cr 'peco_select_history (commandline -b)'
    bind \c] peco_select_ghq_repository
end

function reformatpdftoeps
    find . -name "*.pdf" -print0 | xargs -0 -I '{}' pdftops -f 1 -l 1 -eps '{}' '{}.eps'
end

function renamepdfeps
    find . -name "*.pdf.eps" -print0 | xargs -0 -I '{}' rename 's/(.*\.)pdf.eps/$1eps/' '{}'
end

function replace --argument-names 'before' 'after'
    find . -name "*$before*" | xargs rename -s $before $after
end

function ssh
    command ~/bin/ssh-change-profile.sh $argv
end

function _sirius
    ssh _sirius
end

function _kingkong
    ssh kingkong
end

function __kingkong
     ssh _kingkong
end

function _mdl
    ssh _mdl
end

function _gorilla1
    ssh gorilla1
end

function _gorilla2
    ssh gorilla2
end

function _gorilla3
    ssh gorilla3
end

function _zeus
	ssh zeus
end

function _koike
    ssh _koike
end

function _tanabe
    ssh _tanabe
end

# function e
#     command emacsclient $argv
# end
alias e='emacsclient ""'
alias ee='open ~/drive/github.com/build-emacs-for-macos/builds/Emacs.app'
alias ekill='emacsclient -e "(kill-emacs)"'

function black
    echo -ne "\033]1337;SetProfile=Default\a"
end

function tar_xz
    tar cvJf $argv.tar.xz $argv
end

function tar_gz
    tar cvzf $argv.tar.gz $argv
end

function untar_xz
    tar xvJf $argv
end

function untar_gz
    tar xvzf $argv
end

function gpu
    set num $argv[1]
    set file $argv[2]
    command env CUDA_VISIBLE_DEVICES=$num python $file $argv[3..-1]
end

function en_latex
    command latexmk -e "$bibtex=q/bibtex/" -pdf -pvc $argv
end

function ja_latex
    command latexmk -pvc $argv
end

if test (uname -s) = "Darwin"
    # >>> conda initialize >>>
    # !! Contents within this block are managed by 'conda init' !!
    eval /Users/naoki/miniconda/condabin/conda "shell.fish" "hook" $argv | source
    # <<< conda initialize <<<set -g fish_user_paths "/usr/local/opt/texinfo/bin" $fish_user_paths
    set -g fish_user_paths "/usr/local/opt/icu4c/bin" $fish_user_paths
    set -g fish_user_paths "/usr/local/opt/icu4c/sbin" $fish_user_pathsset -g fish_user_paths "/usr/local/opt/ruby/bin" $fish_user_paths

    set -gx LDFLAGS "-L/usr/local/opt/ruby/lib" $LDFLAGS
    set -gx CPPFLAGS "-I/usr/local/opt/ruby/include" $CPPFLAGS
    set -gx PKG_CONFIG_PATH "/usr/local/opt/ruby/lib/pkgconfig" $PKG_CONFIG_PATH
end

if test -e /Users/naoki/.nix-profile/etc/profile.d/nix.sh
   command sh /Users/naoki/.nix-profile/etc/profile.d/nix.sh
end