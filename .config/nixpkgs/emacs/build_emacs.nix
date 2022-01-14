with rec {
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  stdenv = pkgs.stdenv;
  frameworks = pkgs.darwin.apple_sdk.frameworks;
  withNS = stdenv.isDarwin;
  withX = ! stdenv.isDarwin;
  head = builtins.fetchGit { url = "https://github.com/emacs-mirror/emacs"; ref = "master"; };
};

assert withNS -> !withX;
assert withNS -> stdenv.isDarwin;

stdenv.mkDerivation {
  name = "emacs-head";
  
  # for --native-compilation
  # NATIVE_FULL_AOT = "1";
  LIBRARY_PATH = "${lib.getLib stdenv.cc.libc}/lib";

  enableParallelBuilding = true;

  src = pkgs.fetchFromGitHub {
    owner = "emacs-mirror";
    repo = "emacs";

    rev = "${head.rev}";
    sha256 = "${head.narHash}";
    
    # rev = "4d439744685b6b2492685124994120ebd1fa4abb";
    # sha256 = "00vxb83571r39r0dbzkr9agjfmqs929lhq9rwf8akvqghc412apf";
  };

  patches = if stdenv.isDarwin
            then
              [
                ./patches/fix-window-role.patch
                ./patches/system-appearance.patch
                ./patches/no-frame-refocus-cocoa.patch
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
  #         export NIX_LDFLAGS="-F${frameworks.AppKit}/Library/Frameworks -framework AppKit -F${frameworks.Security}/Library/Frameworks -framework Security -F${frameworks.CoreServices}/Library/Frameworks -framework CoreServices -F${frameworks.CoreFoundation}/Library/Frameworks -framework CoreFoundation $NIX_LDFLAGS";
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
  
  # postInstall =
  #   ''
  #   mkdir -p $out/share/emacs/site-lisp
  #   cp ${./site-start.el} $out/share/emacs/site-lisp/site-start.el
  #   $out/bin/emacs --batch -f batch-byte-compile $out/share/emacs/site-lisp/site-start.el
  #   rm -rf $out/var
  #   rm -rf $out/share/emacs/${version}/site-lisp
  #   mkdir -p $out/Applications
  #   mv nextstep/Emacs.app $out/Applications
  #   '';
}
