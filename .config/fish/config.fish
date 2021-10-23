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

# Theme
set -g theme_color_scheme terminal-dark
set -g fish_prompt_pwd_dir_length 1
set -g theme_display_user yes
set -g theme_hide_hostname no
set -g theme_hostname always


set fish_greeting ""
set -gx TERM xterm-256color

set -xg USER naoki    # A local network user
set -xg HOST gateway.mdl.cs.tsukuba.ac.jp    # Host Name for GIP
set -xg MDL ssh-user@$HOST

set -xg OS (uname -r)
set -xg SYSTEM (uname -s)
set -xg IS_MANJARO (string match '*MANJARO' $OS)
set -xg IS_MAC (string match 'Darwin' $SYSTEM)

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
if test -e $G_REPO/envs/neptune_api_token
    set -xg NEPTUNE_API_TOKEN (cat $G_REPO/envs/neptune_api_token)
end

if test -e /usr/lib/w3m/w3mimagedisplay
    set PATH /usr/lib/w3m/w3mimagedisplay $PATH
end

set MINICONDA (find $HOME -maxdepth 1 -type d -name 'miniconda*' | head -n 1) 
if test -n "$MINICONDA"
    eval $MINICONDA/condabin/conda "shell.fish" "hook" $argv | source
end


################################################################
# Functions and Aliases
################################################################
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

if [ "$INSIDE_EMACS" = 'vterm' ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    end
end

if type -q exa
    alias ll "exa -l -g --icons"
    alias lla "ll -a"
end

################################################################
# Local Config
################################################################
if test -n "$IS_MANJARO"
    source (dirname (status --current-filename))/config-manjaro.fish
else if test -n "$IS_MAC"
    source (dirname (status --current-filename))/config-macos.fish
end
source (dirname (status --current-filename))/functions/my-utility-functions.fish
source (dirname (status --current-filename))/functions/my-server-util.fish
