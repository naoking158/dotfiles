
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
        --with-native-compilation \
        CPPFLAGS=-I/opt/homebrew/opt/ruby/include \
        LDFLAGS=-L/opt/homebrew/opt/ruby/lib
        
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

    echo ""
    echo "Build is Done!"
end
