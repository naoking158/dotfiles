# Available options is here:
# https://rycee.gitlab.io/home-manager/options.html

{ config, pkgs, ... }:

{
  imports = [ ./mail/mail.nix ];
  
  home = {   
    packages = with pkgs; [
      bat
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
      imapnotify
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
      tree
      tree-sitter
      w3m
      wget
      zlib
      zstd
      (pkgs.python3.withPackages(ps: with ps; [pip]))
      (import ./emacs/build_emacs.nix)
    ];
  };
  
  programs = {
    zsh = import ./zsh/zsh.nix { config=config; };
    git = import ./git/git.nix;
    
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };
}
