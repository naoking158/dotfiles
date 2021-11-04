function build_emacs
    if test ! -e "configure"
        ./autogen.sh
    end

    for patch_file in ~/src/emacs_patches/*
        patch -p1 < $patch_file
    end

    export PKG_CONFIG_PATH=/opt/homebrew/opt/pkgconfig:/opt/homebrew/opt/gtk+3/lib/pkgconfig:/opt/homebrew/opt/cairo/lib/pkgconfig:/opt/homebrew/opt/ruby/lib/pkgconfig
    export CPPFLAGS=-I/opt/homebrew/opt/ruby/include:-I/opt/homebrew/opt/imagemagick@6/include
    export LDFLAGS=-L/opt/homebrew/opt/ruby/lib:-L/opt/homebrew/opt/imagemagick@6/lib

    ./configure \
        --with-ns \
        --with-modules \
        --with-xwidgets \
        --with-native-compilation \
        --with-cairo \
        
    make -j(nproc)
    make install
    cp src/emacs nextstep/Emacs.app/Contents/MacOS/bin/

    if test -e ~/src/github.com/emacsfodder/emacs-icons-project
        cd ~/src/github.com/emacsfodder/emacs-icons-project/
        cp EmacsIcon3.icns \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Resources/Emacs.icons
        cp document-icons/*.icns \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Resources/
        mv ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Info.plist \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Info.plist.bak
        cp document-icons/Info.plist \
            ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/Info.plist
    end
end
