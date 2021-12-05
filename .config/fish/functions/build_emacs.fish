
function build_emacs --argument-names 'emacs_version'

    set patch_dir ~/.emacs.d/emacs_patches
    set patch_url "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-28/fix-window-role.patch"
    set patch_url "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-28/system-appearance.patch" $patch_url
    
    if test ! -e "configure"
        ./autogen.sh
    end

    if test ! -e $patch_dir
        mkdir -p $patch_dir
    end

    cd $patch_dir
    for url in $patch_url
        curl -LO $url
    end
    cd -

    for patch_file in $patch_dir/**
        echo "Apply patch: $patch_file"
        patch -p1 < $patch_file
    end

    ./configure \
        --with-ns \
        --with-modules \
        --with-xwidgets \
        --with-native-compilation
        # CPPFLAGS=-I/opt/homebrew/opt/ruby/include \
        # LDFLAGS=-L/opt/homebrew/opt/ruby/lib
        
    gmake -j(nproc) bootstrap

    if test -e ~/src/github.com/emacsfodder/emacs-icons-project
        echo ""
        echo "Change Emacs icons ..."
        cd ~/src/github.com/emacsfodder/emacs-icons-project/
        cp EmacsIcon3.icns \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Resources/Emacs.icons
        cp document-icons/*.icns \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Resources/
        mv ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Info.plist \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Info.plist.bak
        cp document-icons/Info.plist \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Info.plist
        cd -
    end

    gmake install

    echo '#!/usr/bin/env bash

resolve_link() {
  "$(type -p greadlink readlink | head -1)" "$1"
}

abs_dirname() {
  local path="$1"
  local name
  local cwd
  cwd="$(pwd)"

  while [ -n "$path" ]; do
    cd "${path%/*}" || exit 1
    name="${path##*/}"
    path="$(resolve_link "$name" || true)"
  done

  pwd
  cd "$cwd" || exit 1
}

exec "$(dirname "$(abs_dirname "$0")")/Emacs" "$@"' > "./nextstep/Emacs.app/Contents/MacOS/bin/emacs"
    
    echo \n"Build is Done!
Finally, move `./nextstep/Emacs.app` into `/Applications/` and do bellow commands:

    sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacs /usr/local/bin/emacs
    sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient

    "
end


function cleate_emacs_cli_tool
    
end
