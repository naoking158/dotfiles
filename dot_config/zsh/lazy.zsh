# github
export G_USER=$(git config user.name)
export G_ROOT=$(git config ghq.root)
export G_REPO=$(eval echo ${G_ROOT}/github.com/${G_USER})

# Python path
# [[ -e $G_REPO ]] && export PYTHONPATH=$G_REPO

# builtins
function chpwd() {
    if [[ $(pwd) != $HOME ]]; then;
        eza -a --icons
    fi
}

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':completion:*' recent-dirs-insert both
zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-max 1000

# move repositories with peco
function peco-src () {
  local selected_dir=$(ghq list -p | peco --query "$LBUFFER")
  if [ -n "$selected_dir" ]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}
zle -N peco-src
bindkey '^o' peco-src

# search history with peco
function peco-history-selection() {
    BUFFER=`history -n 1 | tac | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N peco-history-selection
bindkey '^R' peco-history-selection

# cdr with peco
function peco-cdr () {
    local selected_dir="$(cdr -l | perl -pe 's/^[0-9]+ +//' | peco --prompt="cdr >" --query "$LBUFFER")"
    if [ -n "$selected_dir" ]; then
	    BUFFER="cd ${selected_dir}"
        CURSOR=$#BUFFER
        zle reset-prompt
    fi
}
zle -N peco-cdr
bindkey '^[^R' peco-cdr

source $HOME/.dotfiles/bin/my-server-util.bash

# rye
if [[ -e "$HOME/.rye/env" ]]; then
    source "$HOME/.rye/env"
fi

# mise
if type mise >/dev/null 2>&1; then
    eval "$($(which mise) activate zsh)"
fi

# Aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias diff='diff --color=auto'
alias dua='/usr/bin/du -shc * | sort -h'
alias e='emacsclient'
alias ed='emacs -nw --daemon'
alias ekill='emacsclient -e "(kill-emacs)"'
alias enw='TERM=xterm-direct emacsclient -nw'
alias eq='emacs -q'
alias eql='eq -l'
alias fd='fd --color=auto --full-path --no-ignore --hidden --exclude ".git"'
alias g='git'
alias gs='g status'
alias gas='g add -A && gs'
alias gc='g commit -m'
alias grep='grep --color=auto'
alias la='eza -a'
alias ll='eza -l -g --icons'
alias lla='ll -a'
alias ls='eza'
alias rg='rg --color=auto --no-ignore --hidden --glob="!.git" --line-number'
alias rm='~/src/github.com/naoking158/rm-alternative/rm-alternative.bash'
alias sudo='sudo '
alias tgz='f() { env COPYFILE_DISABLE=1 tar zcvf $1 --exclude=".DS_Store" ${@:2}; unset -f f; }; f'
alias tree='eza --tree --level 3 -a --ignore-glob "node_modules|.git|.cache" --icons'
alias d='docker'
alias dc='docker compose'
alias dprune='docker system prune -a'

# Keybind - unbind
bindkey -r '^J'

# Global Aliases
alias -g G='| grep'

autoload -Uz compinit && compinit -i

# az.completion
autoload -U bashcompinit && bashcompinit
local ZSH_COMPLETION_PATH=${HOME}/.cache/zsh/completions
if type az > /dev/null 2>&1; then
    if [[ ! -e ${ZSH_COMPLETION_PATH}/az.completion ]]; then
        mkdir -p $ZSH_COMPLETION_PATH
        curl -sL 'https://raw.githubusercontent.com/Azure/azure-cli/dev/az.completion'\
             -o "${ZSH_COMPLETION_PATH}/az.completion"
    fi

    source ${ZSH_COMPLETION_PATH}/az.completion
fi

# Set Go path
if type go >/dev/null 2>&1; then
    export GOPATH=$(go env GOPATH)
    PATH="${PATH}:${GOPATH}/bin"
fi

# setup direnv
if type direnv >/dev/null 2>&1; then
    eval "$(direnv hook zsh)"
fi


if command -v ngrok &>/dev/null; then
    eval "$(ngrok completion)"
fi


# # set global npm bin path
# if type npm >/dev/null 2>&1; then
#     local p
#     p="$(npm prefix --location=global)/bin"
#     PATH="${PATH}:${p}"
# fi

export PATH


typeset -gU path cdpath fpath manpath
