export TERM=xterm-256color
export ZDOTDIR="${HOME}/.config/zsh"

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

export USER="${USERNAME:=sakamoto}"    # A local network user

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

# Set cargo path
[[ -e $HOME/.cargo/env ]] && . "$HOME/.cargo/env"
if [[ -e $HOME/.cargo/bin ]]; then
    PATH="$HOME/.cargo/bin:$PATH"
fi

# Set npm path
if [[ $SYSTEM == "manjaro" ]] && [[ -e "${HOME}/.npm" ]]; then
    npm config set prefix '~/.npm'
    PATH="$HOME/.npm/bin:$PATH"
fi

# Set Go path
if type go >/dev/null 2>&1; then
    export GOPATH=$(go env GOPATH)
    PATH="${PATH}:${GOPATH}/bin"
fi

if type emacs >/dev/null 2>&1; then
    export EDITOR="emacsclient -nw --alternate-editor='emacs -Q -nw'"
else
    export EDITOR=vi
fi

# manjaro
if [[ $SYSTEM == "manjaro" ]]; then
    # Most pure GTK3 apps use wayland by default, but some,
    # like Firefox, need the backend to be explicitely selected.
    export MOZ_ENABLE_WAYLAND=1
    export MOZ_DBUS_REMOTE=1
    export GTK_CSD=0
    
    # qt wayland
    export QT_QPA_PLATFORM="wayland;xcb"
    export QT_QPA_PLATFORMTHEME=qt5ct
    export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
    
    #Java XWayland blank screens fix
    export _JAVA_AWT_WM_NONREPARENTING=1
    
    # set default shell and terminal
    export SHELL=/usr/bin/zsh
    export TERMINAL_COMMAND=/usr/share/sway/scripts/foot.sh
    
    # add default location for zeit.db
    export ZEIT_DB=~/.config/zeit.db
fi

export PATH="/usr/local/bin:$HOME/.local/bin:$PATH"
