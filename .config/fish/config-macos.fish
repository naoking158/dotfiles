eval (/opt/homebrew/bin/brew shellenv)

# Homebrew
set -xg HOMEBREW_EDITOR "emacs -q -nw"

# set PATH /Library/Tex/texbin $PATH
set PATH /usr/local/bin $PATH
# set PATH /usr/local/texlive/2020basic/bin/x86_64-darwin $PATH
set PATH $HOME/.local/bin $PATH
# set PATH /opt/local/bin $PATH

# Node.js
set PATH $HOME/.nodebrew/current/bin $PATH

# Rust
set -U fish_user_paths $fish_user_paths $HOME/.cargo/bin

# fzf
set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_REVERSE_ISEARCH_OPTS "--reverse --height=100%"
set -x FZF_DEFAULT_COMMAND 'rg -a --files --hidden --no-ignore --follow'

set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

function brave
    '/Applications/Brave Browser.app/Contents/MacOS/Brave Browser' $argv    
end

# set -gx LDFLAGS "-L/usr/local/opt/ruby/lib" $LDFLAGS
# set -gx CPPFLAGS "-I/usr/local/opt/ruby/include" $CPPFLAGS
# set -gx PKG_CONFIG_PATH "/usr/local/opt/ruby/lib/pkgconfig" $PKG_CONFIG_PATH
set -gx LDFLAGS "-L/opt/homebrew/opt/ruby/lib" $LDFLAGS
set -gx CPPFLAGS "-I/opt/homebrew/opt/ruby/include" $CPPFLAGS
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/ruby/lib/pkgconfig" $PKG_CONFIG_PATH
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/cairo/lib/pkgconfig" $PKG_CONFIG_PATH
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/gtk+3/lib/pkgconfig" $PKG_CONFIG_PATH
set -gx PKG_CONFIG_PATH "/opt/homebrew/opt/pkgconfig" $PKG_CONFIG_PATH
# set -gx PKG_CONFIG_PATH "/usr/local/lib/pkgconfig" $PKG_CONFIG_PATH
# set -gx PKG_CONFIG_PATH "/usr/local/lib/pkgconfig" $PKG_CONFIG_PATH
# set -gx LIBRARY_PATH (brew --prefix libgccjit)/lib/gcc/11 $LIBRARY_PATH

set -gx LDFLAGS "-L/opt/homebrew/opt/imagemagick@6/lib" $LDFLAGS
set -gx CPPFLAGS "-I/opt/homebrew/opt/imagemagick@6/include" $CPPFLAGS

fish_add_path /opt/homebrew/bin

alias reformatpdftoeps='find . -name "*.pdf" -print0 | xargs -0 -I "{}" pdftops -f 1 -l 1 -eps "{}" "{}.eps"'
alias renamepdfeps='find . -name "*.pdf.eps" -print0 | xargs -0 -I "{}" rename "s/(.*\\.)pdf.eps/$1eps/" "{}"'
