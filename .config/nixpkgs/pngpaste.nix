with rec {
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  frameworks = pkgs.darwin.apple_sdk.frameworks;
  source = builtins.fetchGit { url = "https://github.com/jcsalterego/pngpaste"; ref = "master"; };
};

let
  pngpaste = stdenv.mkDerivation {
    name = "pngpaste";
    src = pkgs.fetchFromGitHub {
      owner = "jcsalterego";
      repo = "pngpaste";
      rev = "${source.rev}";
      sha256 = "${source.narHash}";
    };

    buildInputs = with frameworks; [
      Foundation
      AppKit
      Cocoa
    ];
    
    buildPhase = ''
      make all
      '';

    installPhase = ''
      mkdir -p $out/bin
      cp pngpaste $out/bin/
      chmod +x $out/bin/pngpaste
    '';
};
in pngpaste
