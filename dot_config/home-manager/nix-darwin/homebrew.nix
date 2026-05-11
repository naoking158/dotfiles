{ ... }:

{
  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
    };

    caskArgs = {
      appdir = "~/Applications";
    };

    taps = [
      "d12frosted/emacs-plus"
      "mtgto/macskk"
      
    ];

    brews = [
      "libpq"
      "mysql-client"
    ];

    casks = [
      "bitwarden"
      "ghostty"
      "karabiner-elements"
      "raycast"
      "d12frosted/emacs-plus/emacs-plus-app"
      "mtgto/macskk/macskk"
      "vivaldi"
    ];
  };
}
