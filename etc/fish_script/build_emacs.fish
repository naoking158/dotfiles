
function build_emacs --argument-names 'emacs_version'
    set emacs_dir ~/src/github.com/emacs-mirror/emacs/
    cd $emacs_dir
    git reset --hard HEAD
    git pull origin master

    if test ! -e "configure"
        ./autogen.sh
    end
    
    if test -n "$IS_MAC"
        set MAKE_CMD gmake
        set patch_dir ~/.emacs.d/emacs_patches
        set patch_url "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-28/fix-window-role.patch"
        set patch_url "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-28/system-appearance.patch" $patch_url

        if test ! -e $patch_dir
            mkdir -p $patch_dir
        end

        cd $patch_dir
        for url in $patch_url
            curl -LO $url
        end
        cd $emacs_dir
        
        for patch_file in $patch_dir/**
            echo "Apply patch: $patch_file"
            patch -p1 < $patch_file
        end
    else
        set MAKE_CMD make
    end

    
    ./configure \
        --with-modules \
        --with-xwidgets \
        --with-native-compilation
        # CPPFLAGS=-I/opt/homebrew/opt/ruby/include \
        # LDFLAGS=-L/opt/homebrew/opt/ruby/lib

    $MAKE_CMD -j(nproc) bootstrap
    
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
        cd $emacs_dir
    end

    make install

    if test -n "$IS_MAC"
        cp ~/.dotfiles/etc/helper/emacs-cli.bash ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/MacOS/bin/emacs
        chmod +w ~/src/github.com/emacs-mirror/emacs/nextstep/Emacs.app/Contents/MacOS/bin/emacs
        
        echo \n"Build is Done!
        
        Finally, recommend following steps:

        mv $emacs_dir/nextstep/Emacs.app /Applications/
        sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacs /usr/local/bin/emacs
        sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient

        If necessary, execute following steps before create symlinks:

        sudo unlink /usr/local/bin/emacs
        sudo unlink /usr/local/bin/emacsclient
        "
    end
end
