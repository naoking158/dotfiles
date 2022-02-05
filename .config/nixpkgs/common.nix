# Available options is here:
# https://rycee.gitlab.io/home-manager/options.html

{ config, pkgs, ... }:

let
  extraNodePackages = import ./node/default.nix {};
in
{
  imports = [ ./mail/mail.nix ];
  
  home = {   
    packages = with pkgs; [
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
      hugo
      imagemagick
      imapnotify
      jq
      libgccjit
      librsvg
      libtool
      llvm
      mailutils
      nkf
      nodejs
      extraNodePackages.textlint
      extraNodePackages.textlint-rule-preset-ja-technical-writing
      extraNodePackages.textlint-rule-write-good
      extraNodePackages.textlint-rule-ginger
      extraNodePackages.textlint-rule-alex
      extraNodePackages.textlint-plugin-org
      extraNodePackages.traverse
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
      (pkgs.python3.withPackages(ps: with ps; [pip]))
      (import ./emacs/build_emacs.nix)
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
