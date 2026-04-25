# macOS: Homebrew (GUI apps only, CLI tools are managed by Nix)
if [[ -e /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

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

# move repositories with fzf
function fzf-src () {
  local selected_dir=$(ghq list -p | fzf --query "$LBUFFER")
  if [ -n "$selected_dir" ]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}
zle -N fzf-src
bindkey '^o' fzf-src

# search history with fzf
function fzf-history-selection() {
    BUFFER=`history -n -r 1 | fzf --query "$LBUFFER" --reverse`
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N fzf-history-selection
bindkey '^R' fzf-history-selection

[[ -f "${HOME}/.dotfiles/bin/my-server-util.bash" ]] && source "${HOME}/.dotfiles/bin/my-server-util.bash"

# mise
if type mise >/dev/null 2>&1; then
    eval "$($(which mise) activate zsh)"
    PATH="$HOME/.local/share/mise/shims:$PATH"
fi

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

[[ "$TERM_PROGRAM" == "vscode" ]] && . "$(code --locate-shell-integration-path zsh)"

if command -v ngrok &>/dev/null; then
    eval "$(ngrok completion)"
fi


# set global npm bin path
if type npm >/dev/null 2>&1; then
    local p
    p="$(npm prefix --location=global)/bin"
    PATH="${PATH}:${p}"
fi

export PATH


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
alias rm='~/go/bin/go-to-trash'
alias sudo='sudo '
alias tgz='f() { env COPYFILE_DISABLE=1 tar zcvf $1 --exclude=".DS_Store" ${@:2}; unset -f f; }; f'
alias tree='eza --tree --level 3 -a --ignore-glob "node_modules|.git|.cache" --icons'
alias d='docker'
alias dc='docker compose'
alias dprune='docker system prune -a'


typeset -gU path cdpath fpath manpath
