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

# --- set Nord color ---
set -U fish_color_normal normal
set -U fish_color_command 81a1c1
set -U fish_color_quote a3be8c
set -U fish_color_error ebcb8b
set -U fish_color_redirection b48ead
set -U fish_color_end 88c0d0
set -U fish_color_param eceff4
set -U fish_color_comment 434c5e
set -U fish_color_match --background=brblue
set -U fish_color_selection white --bold --background=brblack
set -U fish_color_search_match bryellow --background=brblack
set -U fish_color_history_current --bold
set -U fish_color_operator 00a6b2
set -U fish_color_escape 00a6b2
set -U fish_color_cwd green
set -U fish_color_cwd_root red
set -U fish_color_valid_path --underline
set -U fish_color_autosuggestion 4c566a
set -U fish_color_user brgreen
set -U fish_color_host normal
set -U fish_color_cancel -r
set -U fish_pager_color_completion normal
set -U fish_pager_color_description B3A06D yellow
set -U fish_pager_color_prefix normal --bold --underline
set -U fish_pager_color_progress brwhite --background=cyan
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++


set -xg USER naoki    # A local network user
set -xg localhome /Users/$USER

set -xg HOST gateway.mdl.cs.tsukuba.ac.jp    # Host Name for GIP
set -xg MDL ssh-user@$HOST

################################################################
# PATH
################################################################
#To solve a locate problem happens in ipython notebook
set -xg LC_ALL en_US.UTF-8
set -xg LANG en_US.UTF-8
# set -xg LANG ja_JP.UTF-8

# github
set -xg G_USER (string split -f2 = (git config -l | grep user.name))
set -xg G_ROOT (string split -f2 = (git config -l | grep ghq.root))
set -xg G_REPO $HOME/src/github.com/naoking158

if test -e $G_REPO
    set -xg PYTHONPATH $G_REPO $PYTHONPATH
end
if test -e $G_REPO/research/neptune_api_token
    set -xg NEPTUNE_API_TOKEN (cat $G_REPO/research/neptune_api_token)
end

if test (uname -s) = "Darwin"
    # Homebrew
    set -xg HOMEBREW_EDITOR "/usr/local/bin/emacs -q -nw"

    set PATH /Library/Tex/texbin $PATH
    set PATH /usr/local/bin $PATH
    set PATH /usr/local/texlive/2020basic/bin/x86_64-darwin $PATH
    set PATH $HOME/.local/bin $PATH
    set PATH /opt/local/bin $PATH

    # Node.js
    set PATH $HOME/.nodebrew/current/bin $PATH

    # Rust
    set -U fish_user_paths $fish_user_paths $HOME/.cargo/bin

    # fzf
    set -U FZF_LEGACY_KEYBINDINGS 0
    set -U FZF_REVERSE_ISEARCH_OPTS "--reverse --height=100%"
    set -x FZF_DEFAULT_COMMAND 'rg -a --files --hidden --no-ignore --follow'

    set -g fish_user_paths "/usr/local/sbin" $fish_user_paths
end

################################################################
# Functions
################################################################
function rm
    argparse -n mycmdname f/force -- $argv
    or return

    if set -lq _flag_f
        /bin/rm -rf $argv
    else
        if ! test -e $HOME/.myTrash
            mkdir $HOME/.myTrash
        end
        mv -i $argv $HOME/.myTrash/
        echo "They are moved to $HOME/.myTrash"
    end
end

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

alias _sirius='ssh sirius'
alias _kingkong='ssh kingkong'
alias __kingkong='ssh _kingkong'
alias _mdl='ssh _mdl'
alias _gorilla1='ssh gorilla1'
alias _gorilla2='ssh gorilla2'
alias _gorilla3='ssh gorilla3'
alias _zeus='ssh zeus'
alias _koike='ssh koike'
alias _tanabe='ssh tanabe'

alias e='emacsclient'
alias ee='open ~/src/emacs28-macOS115/builds/Emacs.app'
alias ekill='emacsclient -e "(kill-emacs)"'

# alias black='echo -ne "\033]1337;SetProfile=Default\a"'

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

set MINICONDA (find $HOME -maxdepth 1 -type d -name 'miniconda*' | head -n 1) 
if test -e $MINICONDA
    eval $MINICONDA/condabin/conda "shell.fish" "hook" $argv | source
end

if test (uname -s) = "Darwin"
    set -gx LDFLAGS "-L/usr/local/opt/ruby/lib" $LDFLAGS
    set -gx CPPFLAGS "-I/usr/local/opt/ruby/include" $CPPFLAGS
    set -gx PKG_CONFIG_PATH "/usr/local/opt/ruby/lib/pkgconfig" $PKG_CONFIG_PATH
end

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end


# check existence and initialize of zoxide command 
if type -q zoxide
		zoxide init fish | source
end
