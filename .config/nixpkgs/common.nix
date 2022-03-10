# Available options is here:
# https://rycee.gitlab.io/home-manager/options.html

{ config, pkgs, ... }:

let
  myEmacs = (import ./emacs/build_emacs.nix);
  glibtool = (import ./libtool/libtool.nix);
  pythonEnv = (pkgs.python3.withPackages(ps: with ps; [
    pip
    setuptools
  ]));
in
{
  imports = [ ./mail/mail.nix ];
  
  home = {   
    packages = with pkgs; [
      glibtool
      myEmacs
      (makeDesktopItem {
        name = "org-protocol";
        exec = "emacsclient %u";
        comment = "Org protocol";
        desktopName = "org-protocol";
        type = "Application";
        mimeType = "x-scheme-handler/org-protocol";
      })
      cairo
      cask
      cmake
      cmigemo
      coreutils
      curl
      exa
      fd
      ffmpeg
      fzf
      gcc
      gh
      ghostscript
      ghq
      glib
      gnutls
      go
      harfbuzz
      htop
      hub
      hugo
      imagemagick
      imapnotify
      jq
      libgccjit
      librsvg
      libxml2
      llvm
      mailutils
      nkf
      nodejs
      nodePackages.textlint
      peco
      php
      pkg-config
      poppler
      proselint
      pythonEnv
      rename
      ripgrep
      rsync
      ruby
      texlab
      tmux
      tree-sitter
      w3m
      wget
      zlib
      zstd
    ];
  };
  
  programs = {
    zsh = import ./zsh/zsh.nix { config=config; };
    git = import ./git/git.nix;
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        theme = "Coldark-Dark";
      };
    };
    
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };
}
