eval (/opt/homebrew/bin/brew shellenv)
fish_add_path /opt/homebrew/bin
fish_add_path /usr/local/bin

# Homebrew
set -xg HOMEBREW_EDITOR "emacsclient -nw -a emacs"

# Node.js
set PATH $HOME/.nodebrew/current/bin $PATH

# Rust
set -U fish_user_paths $fish_user_paths $HOME/.cargo/bin

# fzf
set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_REVERSE_ISEARCH_OPTS "--reverse --height=100%"
set -x FZF_DEFAULT_COMMAND 'rg -a --files --hidden --no-ignore --follow'

function brave
    '/Applications/Brave Browser.app/Contents/MacOS/Brave Browser' $argv    
end

set -gx LDFLAGS "-L/opt/homebrew/opt/ruby/lib" $LDFLAGS
set -gx CPPFLAGS "-I/opt/homebrew/opt/ruby/include" $CPPFLAGS
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/ruby/lib/pkgconfig" $PKG_CONFIG_PATH
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/cairo/lib/pkgconfig" $PKG_CONFIG_PATH
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/gtk+3/lib/pkgconfig" $PKG_CONFIG_PATH
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/pkgconfig" $PKG_CONFIG_PATH
set -gx LDFLAGS "-L/opt/homebrew/opt/imagemagick@6/lib" $LDFLAGS
set -gx CPPFLAGS "-I/opt/homebrew/opt/imagemagick@6/include" $CPPFLAGS

alias reformatpdftoeps='find . -name "*.pdf" -print0 | xargs -0 -I "{}" pdftops -f 1 -l 1 -eps "{}" "{}.eps"'
alias renamepdfeps='find . -name "*.pdf.eps" -print0 | xargs -0 -I "{}" rename "s/(.*\\.)pdf.eps/$1eps/" "{}"'
