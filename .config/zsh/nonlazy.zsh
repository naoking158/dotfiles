# Use emacs keymap as the default.
bindkey -e

#visual editor
autoload -Uz edit-command-line; zle -N edit-command-line
bindkey "^X^E" edit-command-line

# History options should be set in .zshrc and after oh-my-zsh sourcing.
# See https://github.com/nix-community/home-manager/issues/177.
HISTSIZE="10000"
SAVEHIST="10000"

HISTFILE="${HOME}/.local/share/zsh/history"
# mkdir -p "$(dirname '$HISTFILE')"

setopt HIST_FCNTL_LOCK
setopt HIST_IGNORE_DUPS
unsetopt HIST_IGNORE_SPACE
setopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
unsetopt EXTENDED_HISTORY
setopt autocd
