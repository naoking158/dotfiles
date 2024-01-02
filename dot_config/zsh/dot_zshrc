# source command override technique
function source {
  ensure_zcompiled $1
  builtin source $1
}
function ensure_zcompiled {
  local compiled="$1.zwc"
  if [[ ! -r "$compiled" || "$1" -nt "$compiled" ]]; then
    echo "\033[1;36mCompiling\033[m $1"
    zcompile $1
  fi
}
ensure_zcompiled $ZDOTDIR/.zshrc

# Set global environment variables
[[ -f "${HOME}/.profile" ]] && source "${HOME}/.profile"

# sheldon
if ! type sheldon >/dev/null 2>&1; then
    echo "sheldon not found."
    exit 1
fi

export SHELDON_CONFIG_DIR="$ZDOTDIR/sheldon"
sheldon_cache="$SHELDON_CONFIG_DIR/sheldon.zsh"
sheldon_toml="$SHELDON_CONFIG_DIR/plugins.toml"
if [[ ! -r "$sheldon_cache" || "$sheldon_toml" -nt "$sheldon_cache" ]]; then
  sheldon source > $sheldon_cache
fi
source "$sheldon_cache"
unset sheldon_cache sheldon_toml

source $ZDOTDIR/nonlazy.zsh
zsh-defer source $ZDOTDIR/lazy.zsh
zsh-defer unfunction source
