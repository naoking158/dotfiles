{
  enable = true;
  userName = "naoking158";
  userEmail = "naoki@bbo.cs.tsukuba.ac.jp";
  aliases = {
    prettylog = "...";
  };
  extraConfig = {
    core.editor = "vim";
    color.ui = true;
    push.default = "simple";
    pull.ff = "only";
    init.defaultBranch = "main";
    ghq = {
      root = "~/src";
    };
  };

  delta = {
    enable = true;
    options = {
      navigate = true;
      line-numbers = true;
      syntax-theme = "GitHub";
      side-by-side = true;
    };
  };

  ignores = [
    "# Created by https://www.toptal.com/developers/gitignore/api/macos,latex,python,emacs"
    "# Edit at https://www.toptal.com/developers/gitignore?templates=macos,latex,python,emacs"

    "### Emacs ###"
    "# -*- mode: gitignore; -*-"
    "*~"
    "\#*\#"
    "/.emacs.desktop"
    "/.emacs.desktop.lock"
    "*.elc"
    "auto-save-list"
    "tramp"
    ".\#*"

    "# Org-mode"
    ".org-id-locations"
    "*_archive"
    "ltximg/**"

    "# flymake-mode"
    "*_flymake.*"

    "# eshell files"
    "/eshell/history"
    "/eshell/lastdir"

    "# elpa packages"
    "/elpa/"

    "# reftex files"
    "*.rel"

    "# AUCTeX auto folder"
    "/auto/"

    "# cask packages"
    ".cask/"
    "dist/"

    "# Flycheck"
    "flycheck_*.el"

    "# server auth directory"
    "/server/"

    "# projectiles files"
    ".projectile"

    "# directory configuration"
    ".dir-locals.el"

    "# network security"
    "/network-security.data"


    "### LaTeX ###"
    "## Core latex/pdflatex auxiliary files:"
    "*.aux"
    "*.lof"
    "*.log"
    "*.lot"
    "*.fls"
    "*.out"
    "*.toc"
    "*.fmt"
    "*.fot"
    "*.cb"
    "*.cb2"
    ".*.lb"

    "## Intermediate documents:"
    "*.dvi"
    "*.xdv"
    "*-converted-to.*"
    "# these rules might exclude image files for figures etc."
    "*.ps"
    "# *.eps"
    "# *.pdf"

    "## Generated if empty string is given at `Please type another file name for output:`"
    ".pdf"

    "## Bibliography auxiliary files (bibtex/biblatex/biber):"
    "*.bbl"
    "*.bcf"
    "*.blg"
    "*-blx.aux"
    "*-blx.bib"
    "*.run.xml"

    "## Build tool auxiliary files:"
    "*.fdb_latexmk"
    "*.synctex"
    "*.synctex(busy)"
    "*.synctex.gz"
    "*.synctex.gz(busy)"
    "*.pdfsync"

    "## Build tool directories for auxiliary files"
    "# latexrun"
    "latex.out/"

    "## Auxiliary and intermediate files from other packages:"
    "# algorithms"
    "*.alg"
    "*.loa"

    "# achemso"
    "acs-*.bib"

    "# amsthm"
    "*.thm"

    "# beamer"
    "*.nav"
    "*.pre"
    "*.snm"
    "*.vrb"

    "# changes"
    "*.soc"

    "# comment"
    "*.cut"

    "# cprotect"
    "*.cpt"

    "# elsarticle (documentclass of Elsevier journals)"
    "*.spl"

    "# endnotes"
    "*.ent"

    "# fixme"
    "*.lox"

    "# feynmf/feynmp"
    "*.mf"
    "*.mp"
    "*.t[1-9]"
    "*.t[1-9][0-9]"
    "*.tfm"

    "#(r)(e)ledmac/(r)(e)ledpar"
    "*.end"
    "*.?end"
    "*.[1-9]"
    "*.[1-9][0-9]"
    "*.[1-9][0-9][0-9]"
    "*.[1-9]R"
    "*.[1-9][0-9]R"
    "*.[1-9][0-9][0-9]R"
    "*.eledsec[1-9]"
    "*.eledsec[1-9]R"
    "*.eledsec[1-9][0-9]"
    "*.eledsec[1-9][0-9]R"
    "*.eledsec[1-9][0-9][0-9]"
    "*.eledsec[1-9][0-9][0-9]R"

    "# glossaries"
    "*.acn"
    "*.acr"
    "*.glg"
    "*.glo"
    "*.gls"
    "*.*-glg"
    "*.*-glo"
    "*.*-gls"
    "*.glsdefs"
    "*.lzo"
    "*.lzs"

    "# uncomment this for glossaries-extra (will ignore makeindex's style files!)"
    "# *.ist"

    "# gnuplottex"
    "*-gnuplottex-*"

    "# gregoriotex"
    "*.gaux"
    "*.gtex"

    "# htlatex"
    "*.4ct"
    "*.4tc"
    "*.idv"
    "*.lg"
    "*.trc"
    "*.xref"

    "# hyperref"
    "*.brf"

    "# knitr"
    "*-concordance.tex"
    "# TODO Comment the next line if you want to keep your tikz graphics files"
    "*.tikz"
    "*-tikzDictionary"

    "# listings"
    "*.lol"

    "# luatexja-ruby"
    "*.ltjruby"

    "# makeidx"
    "*.idx"
    "*.ilg"
    "*.ind"

    "# minitoc"
    "*.maf"
    "*.mlf"
    "*.mlt"
    "*.mtc"
    "*.mtc[0-9]*"
    "*.slf[0-9]*"
    "*.slt[0-9]*"
    "*.stc[0-9]*"

    "# minted"
    "_minted*"
    "*.pyg"

    "# morewrites"
    "*.mw"

    "# nomencl"
    "*.nlg"
    "*.nlo"
    "*.nls"

    "# pax"
    "*.pax"

    "# pdfpcnotes"
    "*.pdfpc"

    "# sagetex"
    "*.sagetex.sage"
    "*.sagetex.py"
    "*.sagetex.scmd"

    "# scrwfile"
    "*.wrt"

    "# sympy"
    "*.sout"
    "*.sympy"
    "sympy-plots-for-*.tex/"

    "# pdfcomment"
    "*.upa"
    "*.upb"

    "# pythontex"
    "*.pytxcode"
    "pythontex-files-*/"

    "# tcolorbox"
    "*.listing"

    "# thmtools"
    "*.loe"

    "# TikZ & PGF"
    "*.dpth"
    "*.md5"
    "*.auxlock"

    "# todonotes"
    "*.tdo"

    "# vhistory"
    "*.hst"
    "*.ver"

    "# easy-todo"
    "*.lod"

    "# xcolor"
    "*.xcp"

    "# xmpincl"
    "*.xmpi"

    "# xindy"
    "*.xdy"

    "# xypic precompiled matrices and outlines"
    "*.xyc"
    "*.xyd"

    "# endfloat"
    "*.ttt"
    "*.fff"

    "# Latexian"
    "TSWLatexianTemp*"

    "## Editors:"
    "# WinEdt"
    "*.bak"
    "*.sav"

    "# Texpad"
    ".texpadtmp"

    "# LyX"
    "*.lyx~"

    "# Kile"
    "*.backup"

    "# gummi"
    ".*.swp"

    "# KBibTeX"
    "*~[0-9]*"

    "# TeXnicCenter"
    "*.tps"

    "# auto folder when using emacs and auctex"
    "./auto/*"
    "*.el"

    "# expex forward references with \gathertags"
    "*-tags.tex"

    "# standalone packages"
    "*.sta"

    "# Makeindex log files"
    "*.lpz"

    "# REVTeX puts footnotes in the bibliography by default, unless the nofootinbib"
    "# option is specified. Footnotes are the stored in a file with suffix Notes.bib."
    "# Uncomment the next line to have this generated file ignored."
    "#*Notes.bib"

    "### LaTeX Patch ###"
    "# LIPIcs / OASIcs"
    "*.vtc"

    "# glossaries"
    "*.glstex"

    "### macOS ###"
    "# General"
    ".DS_Store"
    ".AppleDouble"
    ".LSOverride"

    "# Icon must end with two \r"
    "Icon"


    "# Thumbnails"
    "._*"

    "# Files that might appear in the root of a volume"
    ".DocumentRevisions-V100"
    ".fseventsd"
    ".Spotlight-V100"
    ".TemporaryItems"
    ".Trashes"
    ".VolumeIcon.icns"
    ".com.apple.timemachine.donotpresent"

    "# Directories potentially created on remote AFP share"
    ".AppleDB"
    ".AppleDesktop"
    "Network Trash Folder"
    "Temporary Items"
    ".apdisk"

    "### Python ###"
    "# Byte-compiled / optimized / DLL files"
    "__pycache__/"
    "*.py[cod]"
    "*$py.class"
    "*.pth"
    "*.npy"
    "epoch_*"

    "!**/journal2021/**/*.npy"


    "# C extensions"
    "*.so"

    "# Distribution / packaging"
    ".Python"
    "build/"
    "develop-eggs/"
    "downloads/"
    "eggs/"
    ".eggs/"
    "parts/"
    "sdist/"
    "var/"
    "wheels/"
    "pip-wheel-metadata/"
    "share/python-wheels/"
    "*.egg-info/"
    ".installed.cfg"
    "*.egg"
    "MANIFEST"
    "*.pickle"

    "# PyInstaller"
    "#  Usually these files are written by a python script from a template"
    "#  before PyInstaller builds the exe, so as to inject date/other infos into it."
    "*.manifest"
    "*.spec"

    "# Installer logs"
    "pip-log.txt"
    "pip-delete-this-directory.txt"

    "# Unit test / coverage reports"
    "htmlcov/"
    ".tox/"
    ".nox/"
    ".coverage"
    ".coverage.*"
    ".cache"
    "nosetests.xml"
    "coverage.xml"
    "*.cover"
    "*.py,cover"
    ".hypothesis/"
    ".pytest_cache/"
    "pytestdebug.log"

    "# Translations"
    "*.mo"
    "*.pot"

    "# Django stuff:"
    "local_settings.py"
    "db.sqlite3"
    "db.sqlite3-journal"

    "# Flask stuff:"
    "instance/"
    ".webassets-cache"

    "# Scrapy stuff:"
    ".scrapy"

    "# Sphinx documentation"
    "docs/_build/"
    "doc/_build/"

    "# PyBuilder"
    "target/"

    "# Jupyter Notebook"
    ".ipynb_checkpoints"

    "# IPython"
    "profile_default/"
    "ipython_config.py"

    "# pyenv"
    ".python-version"

    "# pipenv"
    "#   According to pypa/pipenv#598, it is recommended to include Pipfile.lock in version control."
    "#   However, in case of collaboration, if having platform-specific dependencies or dependencies"
    "#   having no cross-platform support, pipenv may install dependencies that don't work, or not"
    "#   install all needed dependencies."
    "#Pipfile.lock"

    "# poetry"
    "#poetry.lock"

    "# PEP 582; used by e.g. github.com/David-OConnor/pyflow"
    "__pypackages__/"

    "# Celery stuff"
    "celerybeat-schedule"
    "celerybeat.pid"

    "# SageMath parsed files"
    "*.sage.py"

    "# Environments"
    "# .env"
    ".env/"
    ".venv/"
    "env/"
    "venv/"
    "ENV/"
    "env.bak/"
    "venv.bak/"
    "pythonenv*"

    "# Spyder project settings"
    ".spyderproject"
    ".spyproject"

    "# Rope project settings"
    ".ropeproject"

    "# mkdocs documentation"
    "/site"

    "# mypy"
    ".mypy_cache/"
    ".dmypy.json"
    "dmypy.json"

    "# Pyre type checker"
    ".pyre/"

    "# pytype static type analyzer"
    ".pytype/"

    "# operating system-related files"
    "# file properties cache/storage on macOS"
    "*.DS_Store"
    "# thumbnail cache on Windows"
    "Thumbs.db"

    "# profiling data"
    ".prof"

    "# experiments output"
    "*.log"
    "*.logdata"
    "*.dat"

    "# neptune"
    ".neptune"

    "# Compressed files"
    "*.tar.gz"
    "*.tar"
    "*.gz"
    "*.zip"
    "*.gzip"

    "# specified data"
    "**/data/CIFAR10/cifar-10-batches-py/*"
    "**/data/MNIST/raw/*"
    
    "# End of https://www.toptal.com/developers/gitignore/api/macos,latex,python,emacs"
  ];
}
