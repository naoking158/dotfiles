# export XDG_CONFIG_HOME=$HOME/.config

export TERM=xterm-256color

export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

export USER=naoki    # A local network user
export HOST=gateway.mdl.cs.tsukuba.ac.jp    # Host Name for GIP
export MDL=ssh-user@$HOST

export MY_CONFIG_PATH=".dotfiles/.config"
export MY_BASH_PATH="${MY_CONFIG_PATH}/bash"

if [[ $(uname -r) =~ .*MANJARO ]]; then
    export OS='manjaro';
elif [[ $(uname -s) =~ Darwin ]]; then
    export OS='macos';
else
    export OS='ubuntu';
fi

#To solve a locate problem happens in ipython notebook
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# github
G_USER=$(git config user.name)
G_ROOT=$(git config ghq.root)
G_REPO=$(eval echo ${G_ROOT}/github.com/${G_USER})

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

# if [[ "$OS" = 'macos' ]]; then
#     [[ -e $MY_BASH_PATH/.bash_macos ]] && {
#         . "$MY_BASH_PATH/.bash_macos"
#     }
# elif [[ "$OS" = 'manjaro' ]]; then
#     [[ -e $MY_BASH_PATH/.bash_manjaro ]] && {
#         . "$MY_BASH_PATH/.bash_manjaro"
#     }
# fi

[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && {
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
}

[[ -e $HOME/.nix-defexpr ]] && {
    export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
}

[[ -e $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh ]] && {
    . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
}

export PATH
