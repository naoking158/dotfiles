# This code is almost same as code of following URL: 
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/emacs/generic.nix

with rec {
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  stdenv = pkgs.stdenv;
  frameworks = pkgs.darwin.apple_sdk.frameworks;
  
  withNS = stdenv.isDarwin;
  withX = ! stdenv.isDarwin;
  siteStart = ./site-start.el;
  # emacs-source = builtins.fetchGit { url = "https://github.com/emacs-mirror/emacs"; ref = "emacs-28"; };
};

assert withNS -> !withX;
assert withNS -> stdenv.isDarwin;

let
  emacs = stdenv.mkDerivation {
    name = "emacs-head";
    
    # for --native-compilation
    NATIVE_FULL_AOT = "1";
    LIBRARY_PATH = "${lib.getLib stdenv.cc.libc}/lib";
    
    enableParallelBuilding = true;

    src = pkgs.fetchFromGitHub {
      owner = "emacs-mirror";
      repo = "emacs";

      # rev = "${emacs-source.rev}";
      # sha256 = "${emacs-source.narHash}";

      # emacs-29
      # rev = "015d881b6dbdd527e309d8337daaf22d192a3c70";
      # sha256 = "sha256-a36MJb/M4Iyxxw8hGhOv/j5xA/KCE02XiqZObOQXqk0=";

      # emacs-28
      rev = "5942504391df1b81ded820f9c8cd3047a05f3543";
      sha256 = "sha256-JaYvA1Uh22T/cCqAfeGPxN0q17hR/q95AAkbOkAiKSQ=";

      
    };

    patches = if stdenv.isDarwin
              then
                [
                  ./patches/fix-window-role.patch
                  ./patches/system-appearance.patch
                  # ./patches/no-frame-refocus-cocoa.patch
                ]
              else [];

    postPatch = lib.concatStringsSep "\n" [
      ''
      substituteInPlace src/Makefile.in --replace 'RUN_TEMACS = ./temacs' 'RUN_TEMACS = env -i ./temacs'
      substituteInPlace lisp/international/mule-cmds.el --replace /usr/share/locale ${pkgs.gettext}/share/locale
      for makefile_in in $(find . -name Makefile.in -print); do
          substituteInPlace $makefile_in --replace /bin/pwd pwd
      done
      ''
      
      # Make native compilation work both inside and outside of nix build
      (let
        backendPath = (lib.concatStringsSep " "
          (builtins.map (x: ''\"-B${x}\"'') [
            # Paths necessary so the JIT compiler finds its libraries:
            "${lib.getLib pkgs.libgccjit}/lib"
            "${lib.getLib pkgs.libgccjit}/lib/gcc"
            "${lib.getLib stdenv.cc.libc}/lib"

            # Executable paths necessary for compilation (ld, as):
            "${lib.getBin stdenv.cc.cc}/bin"
            "${lib.getBin stdenv.cc.bintools}/bin"
            "${lib.getBin stdenv.cc.bintools.bintools}/bin"
          ]));
      in ''
        substituteInPlace lisp/emacs-lisp/comp.el --replace \
          "(defcustom native-comp-driver-options nil" \
          "(defcustom native-comp-driver-options '(${backendPath})"
      '')
      ""
    ];

    # shellHook = (
    #   if pkgs.stdenv.isDarwin then
    #     ''
    #     export NIX_LDFLAGS="-F${frameworks.AppKit}/Library/Frameworks -framework AppKit -F${frameworks.Security}/Library/Frameworks -framework Security -F${frameworks.CoreServices}/Library/Frameworks -framework CoreServices -F${frameworks.CoreFoundation}/Library/Frameworks -framework CoreFoundation $NIX_LDFLAGS";
    #     ''
    #   else
    #     ""
    # );

    buildInputs = with pkgs; [
      autoconf texinfo ncurses # --without-all
      gnutls pkg-config # normal build
      jansson # --with-json
      zlib libgccjit # --with-native-compilation
      imagemagick
      libxml2
    ] ++
    (lib.optionals stdenv.isDarwin [ # https://github.com/holochain/launcher/issues/37
      frameworks.AppKit
      frameworks.IOKit
      frameworks.Foundation
      frameworks.GSS
      frameworks.ImageIO
      darwin.sigtool
    ]
    );

    preConfigure = "./autogen.sh";
    configureFlags = [
      "--disable-build-details" # for a (more) reproducible build
      "--with-modules"
      "--with-json"
      "--with-native-compilation"
      "--with-pgtk"
      "--with-imagemagick"
      "--with-xml2"
    ] ++
    (lib.optional stdenv.isDarwin
      (lib.withFeature withNS "ns")) ++
    (if withNS then
      [ "--disable-ns-self-contained" ]
     else if withX then
       [ "--with-xft" "--with-cairo" ]
     else
       [ "--with-x=no" "--with-xpm=no" "--with-jpeg=no" "--with-png=no"
         "--with-gif=no" "--with-tiff=no" ]
    );
    
    postInstall = ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${siteStart} $out/share/emacs/site-lisp/site-start.el
      $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/site-start.el
      siteVersionDir=`ls $out/share/emacs | grep -v site-lisp | head -n 1`
      rm -r $out/share/emacs/$siteVersionDir/site-lisp
    '' + lib.optionalString withNS ''
      mkdir -p $out/Applications
      mv nextstep/Emacs.app $out/Applications
      ln -snf $out/lib/emacs/*/native-lisp $out/Applications/Emacs.app/Contents/native-lisp
    '' + ''
      echo "Generating native-compiled trampolines..."
      # precompile trampolines in parallel, but avoid spawning one process per trampoline.
      # 1000 is a rough lower bound on the number of trampolines compiled.
      $out/bin/emacs --batch --eval "(mapatoms (lambda (s) \
        (when (subr-primitive-p (symbol-function s)) (print s))))" \
        | xargs -n $((1000/NIX_BUILD_CORES + 1)) -P $NIX_BUILD_CORES \
          $out/bin/emacs --batch -l comp --eval "(while argv \
            (comp-trampoline-compile (intern (pop argv))))"
      mkdir -p $out/share/emacs/native-lisp
      $out/bin/emacs --batch \
        --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp\")" \
        -f batch-native-compile $out/share/emacs/site-lisp/site-start.el
    '';
  };
in emacs
