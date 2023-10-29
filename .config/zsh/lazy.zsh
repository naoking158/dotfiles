if type brew &>/dev/null; then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

  autoload -Uz compinit && compinit

  autoload -U bashcompinit && bashcompinit

  if [[ -e $(brew --prefix)/etc/bash_completion.d/az ]]; then
      source $(brew --prefix)/etc/bash_completion.d/az
  fi
fi

# builtins
function chpwd() {
    if [[ $(pwd) != $HOME ]]; then;
        exa -a --icons
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
alias la='exa -a'
alias ll='exa -l -g --icons'
alias lla='ll -a'
alias ls='exa'
alias rg='rg --color=auto --no-ignore --hidden --glob="!.git" --line-number'
alias rm='~/src/github.com/naoking158/rm-alternative/rm-alternative.bash'
alias sudo='sudo '
alias tgz='f() { env COPYFILE_DISABLE=1 tar zcvf $1 --exclude=".DS_Store" ${@:2}; unset -f f; }; f'
alias tree='exa --tree --level 3 -a --ignore-glob "node_modules|.git|.cache" --icons'
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