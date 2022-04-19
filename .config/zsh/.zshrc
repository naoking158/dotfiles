# Set global environment variables
[[ -f "${HOME}/.profile" ]] && source "${HOME}/.profile"


typeset -U path cdpath fpath manpath

# for profile in ${(z)NIX_PROFILES}; do
#   fpath+=($profile/share/zsh/site-functions $profile/share/zsh/$ZSH_VERSION/functions $profile/share/zsh/vendor-completions)
# done

# HELPDIR="/nix/store/q7bjpj8q0jx429nna2zpnzw5z8k946q5-zsh-5.8/share/zsh/$ZSH_VERSION/help"

# Use emacs keymap as the default.
bindkey -e

declare -A ZINIT

##############################################################
# ZINIT https://github.com/zdharma-continuum/zinit
##############################################################
ZINIT[HOME_DIR]="$XDG_CACHE_HOME/zsh/zinit"
ZINIT[BIN_DIR]="$ZINIT[HOME_DIR]/bin"
ZINIT[PLUGINS_DIR]="$ZINIT[HOME_DIR]/plugins"
ZINIT[ZCOMPDUMP_PATH]="$XDG_CACHE_HOME/zsh/zcompdump"
# export ZINIT[OPTIMIZE_OUT_DISK_ACCESSES]=1
export ZPFX="$ZINIT[HOME_DIR]/polaris"

local __ZINIT="$ZINIT[BIN_DIR]/zinit.zsh"

if [[ ! -f "$__ZINIT" ]]; then
  if (( $+commands[git] )); then
    git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT[BIN_DIR]"
  else
    echo 'git not found' >&2
    exit 1
  fi
fi

source "$__ZINIT"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Utilities & enhancements {{{
  zinit ice wait lucid
  zinit light https://github.com/zsh-users/zsh-history-substring-search
  # bind UP and DOWN keys
  bindkey "${terminfo[kcuu1]}" history-substring-search-up
  bindkey "${terminfo[kcud1]}" history-substring-search-down

  # bind UP and DOWN arrow keys (compatibility fallback)
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
# }}}

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
# zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/chpwd-recent-dirs"

#visual editor
autoload -Uz edit-command-line; zle -N edit-command-line
# bindkey -M vicmd v edit-command-line
bindkey "^X^E" edit-command-line

# zinit plugins
zinit wait lucid for \
     atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
         zdharma-continuum/fast-syntax-highlighting \
     atload"!_zsh_autosuggest_start" \
         zsh-users/zsh-autosuggestions \
     blockf \
         zsh-users/zsh-completions \
     as"blockf; completion; snippet" \
         https://github.com/esc/conda-zsh-completion/blob/master/_conda \

zinit wait lucid for \
    atclone"curl -sOL https://github.com/ohmyzsh/ohmyzsh/raw/master/plugins/tmux/tmux.extra.conf" \
        OMZP::tmux

zinit ice pick"bd.zsh"; zinit light Tarrasch/zsh-bd

zinit ice atclone"dircolors -b LS_COLORS > clrs.zsh" \
    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”'
zinit light trapd00r/LS_COLORS

zinit ice depth=1; zinit light romkatv/powerlevel10k
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

zinit lucid from:'gh-r' \
        as:'program' \
        pick:'delta*/delta' \
        light-mode \
        for @dandavison/delta

zinit ice from"gh-r" as"command"
zinit light junegunn/fzf
zinit ice lucid as"command" id-as"junegunn/fzf-tmux" pick"bin/fzf-tmux"
zinit light junegunn/fzf
zinit ice lucid multisrc"shell/{completion,key-bindings}.zsh" id-as"junegunn/fzf_completions" pick"/dev/null"
zinit light junegunn/fzf

zinit ice from"gh-r" as"command" pick"*/ghq"
zinit light x-motemen/ghq

zinit light mollifier/anyframe
bindkey '^x^b' anyframe-widget-cdr
bindkey '^x^f' anyframe-widget-insert-filename
bindkey '^x^g' anyframe-widget-cd-ghq-repository
bindkey '^x^k' anyframe-widget-kill
bindkey '^x^r' anyframe-widget-put-history
zstyle ":anyframe:selector:" use fzf-tmux


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

# nix home-manager functions
# function home-update () {
#     case "$SYSTEM" in
#         "macos" ) home-manager switch -f $XDG_CONFIG_HOME/nixpkgs/macos.nix ;;
#         * ) home-manager switch -f $XDG_CONFIG_HOME/nixpkgs/linux.nix ;;
#     esac
#  }

#  function home-package () {
#      if [[ $# = 0 ]]; then
#         home-manager packages
#     else
#         home-manager packages | grep $@
#     fi
# }




# Oh-My-Zsh/Prezto calls compinit during initialization,
# calling it twice causes slight start up slowdown
# as all $fpath entries will be traversed again.






# Environment variables
# . "/Users/naoki/.nix-profile/etc/profile.d/hm-session-vars.sh"








# History options should be set in .zshrc and after oh-my-zsh sourcing.
# See https://github.com/nix-community/home-manager/issues/177.
HISTSIZE="10000"
SAVEHIST="10000"

HISTFILE="${HOME}/.local/share/zsh/history"
mkdir -p "$(dirname "$HISTFILE")"

setopt HIST_FCNTL_LOCK
setopt HIST_IGNORE_DUPS
unsetopt HIST_IGNORE_SPACE
setopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
unsetopt EXTENDED_HISTORY
setopt autocd

source $HOME/.dotfiles/bin/my-server-util.bash

# powerlevel10k-instant-prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/sakamoto/miniconda/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/sakamoto/miniconda/etc/profile.d/conda.sh" ]; then
        . "/Users/sakamoto/miniconda/etc/profile.d/conda.sh"
    else
        export PATH="/Users/sakamoto/miniconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# Aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias __gorilla1='ssh _gorilla1'
alias __gorilla2='ssh _gorilla2'
alias __gorilla3='ssh _gorilla3'
alias __gorilla4='ssh _gorilla4'
alias __gorilla5='ssh _gorilla5'
alias __kingkong='ssh _kingkong'
alias __zeus='ssh _zeus'
alias _gorilla1='ssh gorilla1'
alias _gorilla2='ssh gorilla2'
alias _gorilla3='ssh gorilla3'
alias _gorilla4='ssh gorilla4'
alias _gorilla5='ssh gorilla5'
alias _kingkong='ssh kingkong'
alias _mdl='ssh _mdl'
alias _zeus='ssh zeus'
alias c='conda'
alias ca='c activate'
alias cat='bat'
alias ce='c env'
alias ci='c install'
alias cs='c search'
alias diff='diff --color=auto'
alias dua='/usr/bin/du -shc * | sort -h'
alias e='emacsclient'
alias ed='emacs -nw --daemon'
alias ekill='emacsclient -e "(kill-emacs)"'
alias en_latex='latexmk -e "$bibtex=q/bibtex/" -pdf -pvc'
alias enw='emacsclient -nw'
alias eq='emacs -q'
alias eql='eq -l'
alias fd='fd --color=auto --full-path --no-ignore --hidden --exclude ".git"'
alias g='git'
alias gas='g add -A && gs'
alias gc='g commit -m'
alias grep='grep --color=auto'
alias gs='g status'
alias ja_latex='latexmk -pvc'
alias la='exa -a'
alias ll='exa -l -g --icons'
alias lla='ll -a'
alias ls='exa --icons'
# alias nc-list='nix-channel --list'
# alias nc-update='nix-channel --update'
# alias ne-search='nix-env -qa'
alias rg='rg --color=auto --no-ignore --hidden --glob="!.git" --line-number'
alias rm='~/src/github.com/naoking158/rm-alternative/rm-alternative.bash'
alias sudo='sudo '
alias tgz='f() { env COPYFILE_DISABLE=1 tar zcvf $1 --exclude=".DS_Store" ${@:2}; unset -f f; }; f'
alias tree='exa --tree --level 3 -a --ignore-glob "node_modules|.git|.cache" --icons'

# Global Aliases
alias -g G='| grep'

# Named Directory Hashes

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
