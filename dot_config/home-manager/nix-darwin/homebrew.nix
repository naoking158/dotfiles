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

    # NOTE: third-party taps below must also be listed in
    # `dot_homebrew/trust.json`. Homebrew 6.0+ enforces
    # HOMEBREW_REQUIRE_TAP_TRUST, and nix-darwin's brew bundle activation
    # invocation strips XDG_CONFIG_HOME, so brew reads
    # ~/.homebrew/trust.json (not ~/.config/homebrew/trust.json).
    # See docs/analysis/bugs/20260628-brew-tap-trust.md.
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
      "sequel-ace"
      "vivaldi"
    ];
  };
}
