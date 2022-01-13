{
  stdenv, lib,
  autoconf, texinfo, ncurses,
  gnutls, pkg-config,
  jansson,
  zlib, libgccjit,
  cairo,
  withNS ? stdenv.isDarwin,
  withX ? ! stdenv.isDarwin
}:

stdenv.mkDerivation rec {
  name = "emacs-head";

  src = fetchFromGitHub {
    owner = "emacs-mirror";
    repo = "emacs";

    rev = "4d439744685b6b2492685124994120ebd1fa4abb";
    sha256 = "00vxb83571r39r0dbzkr9agjfmqs929lhq9rwf8akvqghc412apf";
  };

  enableParallelBuilding = true;
  
  # for --native-compilation
  NATIVE_FULL_AOT = "1";
  LIBRARY_PATH = "${pkgs.stdenv.cc.libc}/lib";

  buildInputs = with pkgs; [
    autoconf texinfo ncurses # --without-all
    gnutls pkg-config # normal build
    jansson # --with-json
    zlib libgccjit # --with-native-compilation
  ];

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
            "--with-gif=no" "--with-tiff=no" ])
}
