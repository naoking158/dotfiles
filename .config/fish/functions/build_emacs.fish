function build_emacs
    if test ! -e "configure"
        ./autogen.sh
    end

    for patch_file in ~/.emacs.d/emacs_patches/*
        echo "Apply patch: $patch_file ..."
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
    gmake install

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

    echo ""
    echo "Build is Done!"
end
