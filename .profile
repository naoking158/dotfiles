export TERM=xterm-256color
export ZDOTDIR="${HOME}/.config/zsh"

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

export USER=naoki    # A local network user
export MDL=ssh-user@gateway.mdl.cs.tsukuba.ac.jp

export MY_CONFIG_PATH=".dotfiles/.config"
export MY_BASH_PATH="${MY_CONFIG_PATH}/bash"

if [[ $(uname -r) =~ .*MANJARO ]]; then
    export SYSTEM='manjaro';
elif [[ $(uname -s) =~ Darwin ]]; then
    export SYSTEM='macos';
else
    export SYSTEM='ubuntu';
fi

#To solve a locate problem happens in ipython notebook
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# github
export G_USER=$(git config user.name)
export G_ROOT=$(git config ghq.root)
export G_REPO=$(eval echo ${G_ROOT}/github.com/${G_USER})

# Python path
[[ -e $G_REPO ]] && export PYTHONPATH=$G_REPO

# Neptune api token
[[ -e $G_REPO/envs/neptune_api_token ]] && {
    export NEPTUNE_API_TOKEN=$(cat $G_REPO/envs/neptune_api_token);
}

# Miniconda path
if [[ -e $HOME/miniconda ]]; then
    export MINICONDA=$HOME/miniconda
elif [[ -e $HOME/miniconda3 ]]; then
    export MINICONDA=$HOME/miniconda3
fi

# Set cargo path
[[ -e $HOME/.cargo/env ]] && . "$HOME/.cargo/env"


#[[ -e $HOME/.nix-defexpr ]] && {
#    export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH};
#}

#[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && {
#   . "$HOME/.nix-profile/etc/profile.d/nix.sh";
#}

#[[ -e $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh ]] && {
#    . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh";
#}

if [[ $SYSTEM == "macos" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi
export PATH="/usr/local/bin:$HOME/.local/bin:$PATH"

if type emacs >/dev/null 2>&1; then
    export EDITOR="emacsclient -nw --alternate-editor='emacs -Q -nw'"
else
    export EDITOR=vi
fi
