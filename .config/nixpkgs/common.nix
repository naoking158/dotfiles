{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    cairo
    cask
    cmigemo
    coreutils
    curl
    exa
    fd
    fzf
    gcc
    gh
    ghostscript
    ghq
    gnutls
    go
    harfbuzz
    htop
    hugo
    imagemagick
    jq
    libgccjit
    librsvg
    llvm
    mailutils
    nkf
    nodejs
    peco
    php
    pkg-config
    poppler
    proselint
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
}
