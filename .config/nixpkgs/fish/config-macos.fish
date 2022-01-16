
# fzf
set -U FZF_LEGACY_KEYBINDINGS 0
set -U FZF_REVERSE_ISEARCH_OPTS "--reverse --height=100%"
set -x FZF_DEFAULT_COMMAND 'rg -a --files --hidden --no-ignore --follow'

# # Homebrew
# set -g HOMEBREW_EDITOR "emacsclient -nw -a emacs"


# # Node.js
# fish_add_path -g $HOME/.nodebrew/current/bin

# # Homebrew
# fish_add_path -g /opt/homebrew/bin

# # Option `-m` for checking first
# fish_add_path -mg /usr/local/bin


# set -g PKG_CONFIG_PATH "/opt/homebrew/opt/ruby/lib/pkgconfig" $PKG_CONFIG_PATH
# set -g PKG_CONFIG_PATH "/opt/homebrew/opt/cairo/lib/pkgconfig" $PKG_CONFIG_PATH
# set -g PKG_CONFIG_PATH "/opt/homebrew/opt/gtk+3/lib/pkgconfig" $PKG_CONFIG_PATH
# set -g PKG_CONFIG_PATH "/opt/homebrew/opt/pkgconfig" $PKG_CONFIG_PATH
# set -g LDFLAGS "-L/opt/homebrew/opt/ruby/lib" $LDFLAGS
# set -g CPPFLAGS "-I/opt/homebrew/opt/ruby/include" $CPPFLAGS
# set -g LDFLAGS "-L/opt/homebrew/opt/imagemagick@6/lib" $LDFLAGS
# set -g CPPFLAGS "-I/opt/homebrew/opt/imagemagick@6/include" $CPPFLAGS


alias reformatpdftoeps='find . -name "*.pdf" -print0 | xargs -0 -I "{}" pdftops -f 1 -l 1 -eps "{}" "{}.eps"'
alias renamepdfeps='find . -name "*.pdf.eps" -print0 | xargs -0 -I "{}" rename "s/(.*\\.)pdf.eps/$1eps/" "{}"'

function brave
    '/Applications/Brave Browser.app/Contents/MacOS/Brave Browser' $argv    
end
