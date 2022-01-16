{ config, pkgs, ... }:

{
  imports = [ ./common.nix ];

  home = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "naoki";
    homeDirectory = "/Users/naoki";

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "22.05";
    sessionVariables = {
      EDITOR = "emacs";
    };

    # Packages
    packages = with pkgs; [
      gtk3-x11
      (import ./emacs/build_emacs.nix)
    ];
  };

  programs = {
    zsh = import ./zsh/zsh.nix { config=config; pkgs=pkgs; }; 
    
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };
}
