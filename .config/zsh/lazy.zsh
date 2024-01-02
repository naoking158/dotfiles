# github
export G_USER=$(git config user.name)
export G_ROOT=$(git config ghq.root)
export G_REPO=$(eval echo ${G_ROOT}/github.com/${G_USER})

# Python path
[[ -e $G_REPO ]] && export PYTHONPATH=$G_REPO

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
alias pull3='ssh vm3 "bash ~/work/VMOperateTool/utils/download.sh" && myrsync -d vm3 ~/work/VMOperateTool/ ~/src/github.com/fixpoint/VMOperateTool/'
alias push3='ssh vm3 "rm -rf ~/work/VMOperateTool" && myrsync -u vm3 ~/src/github.com/fixpoint/VMOperateTool/ ~/work/VMOperateTool/'
alias pull4='ssh vm4 "bash ~/work/VMOperateTool/utils/download.sh" && myrsync -d vm4 ~/work/VMOperateTool/ ~/src/github.com/fixpoint/VMOperateTool/'
alias push4='ssh vm4 "rm -rf ~/work/VMOperateTool" && myrsync -u vm4 ~/src/github.com/fixpoint/VMOperateTool/ ~/work/VMOperateTool/'
# alias deploy='ssh vm3 "rm -rf ~/work/VMOperateTool" && myrsync -u vm3 ~/src/work/VMOperateTool/ ~/work/VMOperateTool/ && ssh vm3 "bash ~/work/VMOperateTool/utils/upload.sh"'
# alias push='ssh vm5 "rm -rf ~/work/ToolsOnKE" && myrsync -u vm5 ~/src/github.com/fixpoint/ToolsOnKE/ ~/work/ToolsOnKE/'
# alias xargs='gxargs'

dev_vm="vm4"
dev_pkg="VMOperateTool"
alias dpull='ssh ${dev_vm} "bash ~/work/${dev_pkg}/utils/download.sh" && myrsync -d ${dev_vm} ~/work/${dev_pkg}/ ~/src/github.com/fixpoint/${dev_pkg}/'
alias dpush='ssh ${dev_vm} "rm -rf ~/work/${dev_pkg}" && myrsync -u ${dev_vm} ~/src/github.com/fixpoint/${dev_pkg}/ ~/work/${dev_pkg}/'

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

# Set cargo path
[[ -e $HOME/.cargo/env ]] && . "$HOME/.cargo/env"
if [[ -e $HOME/.cargo/bin ]]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# Set npm path
if [[ -e "${HOME}/.npm" ]]; then
    npm config set prefix '~/.npm'
    PATH="$HOME/.npm/bin:$PATH"
    NODE_PATH="$HOME/.npm/lib/node_modules"
fi

# Set Go path
if type go >/dev/null 2>&1; then
    export GOPATH=$(go env GOPATH)
    PATH="${PATH}:${GOPATH}/bin"
fi
export PATH


typeset -U path cdpath fpath manpath
