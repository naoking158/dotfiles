{ pkgs ? import <nixpkgs> {} }:

let
  common = import ./common.nix {};
  macos_pkgs = with common;
    [
      nixpkgs.gtk3-x11
      nixpkgs.webkit
    ];
in macos_pkgs
    
