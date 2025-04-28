#!/usr/bin/env bash
#
# Usage:
#     $0 [OPTION]...
#     $0 -j=1 -v=28 --prefix=~/.local --working_dir=~/src/emacs  (same without options)
#
# Options:
#     -h, --help
#     -j, --parallel COUNT    ----    Compile using COUNT parallel processes (default: 1).
#     -v, --version NUMBER    ----    Emacs version (default: 28).
#         --working_dir DIR   ----    Working directory to be used for downloading sources and building them (default: "~/src/emacs").
#         --source DIR        ----    Source directory that contains build files and `autogen.sh` (default: It downloaded in working_dir).
#         --prefix DIR        ----    Directory where the installation process should put emacs and its data files (default: "~/.local").
#

function help () {
    awk -v CMD="$(basename $0)" 'NR > 2 {
    if (/^#/) {
        sub("^# ?", "");
        sub("\\$0", CMD);
        print }
    else { exit }
    }' $0
    exit 1
}

set -Ceu
PATCH_URL=(
    "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-28/fix-window-role.patch"
    "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-30/system-appearance.patch"
)

function e_error() {
    printf " \033[31m%s\033[m\n" "✖ $*" 1>&2
}

function e_newline() {
    printf "\n"
}

function e_arrow() {
    printf " \033[37;1m%s\033[m\n" "➜ $*"
}

function e_indent() {
    for ((i=0; i<${1:-4}; i++)); do
        echon " "
    done
    if [ -n "$2" ]; then
        echo "$2"
    else
        cat <&0
    fi
}

function e_header() {
    printf " \033[37;1m%s\033[m\n" "$*"
}

function e_important() {
    printf " \033[31;1m%s\033[m\n" "$*"
}

function e_done() {
    printf " \033[37;1m%s\033[m...\033[32mOK\033[m\n" "✔ $*"
}

function is_macos() { [[ $(uname -s) =~ Darwin ]]; }
function is_manjaro() { [[ $(uname -r) =~ .*MANJARO ]]; }

# Parse arguments
while (( $# > 0 )); do
    case $1 in
        -h | -help | --help)
            help
            ;;
        -j | --parallel | -j=* | --parallel=*)
            if [[ "$1" =~ ^-j= || "$1" =~ ^--parallel= ]]; then
                NPROC="${1##*=}"
            elif [[ -z "${2:-}" || "$2" =~ ^-+ ]]; then
                e_error "'-j/--parallel' requires an int number."
                exit 1
            else
                NPROC="$2"
            fi
            ;;
        -v | --version | -v=* | --version=*)
            if [[ "$1" =~ ^-v= || "$1" =~ ^--version= ]]; then
                VERSION="${1##*=}"
            elif [[ -z "${2:-}" || "$2" =~ ^-+ ]]; then
                e_error "'-v/--version' requires an int number."
                exit 1
            else
                VERSION="$2"
            fi
            ;;
        --working_dir | --working_dir=*)
            if [[ "$1" =~ ^--working_dir ]]; then
                WORK_DIR="${1##*=}"
            elif [[ -z "${2:-}" || "$2" =~ ^-+ ]]; then
                e_error "'--working_dir' requires directory name to be used for downloading sources and building them."
                exit 1
            else
                WORK_DIR="$2"
            fi
            ;;
        --source | --source=*)
            if [[ "$1" =~ ^--source= ]]; then
                SOURCE_DIR="${1##*=}"
            elif [[ -z "${2:-}" || "$2" =~ ^-+ ]]; then
                e_error "'--source' requires directory name that contains build files and 'autogen.sh'."
                exit 1
            else
                SOURCE_DIR="$2"
            fi
            ;;
        --prefix | --prefix=*)
            if [[ "$1" =~ ^--prefix= ]]; then
                PREFIX="${1##*=}"
            elif [[ -z "${2:-}" || "$2" =~ ^-+ ]]; then
                e_error "'--prefix' requires directory name where the installation process should put emacs and its data files."
            else
                PREFIX="$2"
            fi
            ;;
        -*)
            e_error "Illegal option -- '$(echo $1 | sed 's/^-*//')'."
            help
            ;;
    esac
    shift
done

declare -r  ORIGIN=$(pwd)
declare -ri NPROC="${NPROC:=1}"
if [[ "${VERSION:-}" ]]; then
    if [[ $VERSION == "31" || $VERSION == "latest" ]]; then
        VERSION="master"
    else
        VERSION="emacs-${VERSION}"
    fi
else
    VERSION="emacs-30"
fi
declare -r VERSION=$VERSION
declare -r  WORK_DIR="${WORK_DIR:=${HOME}/src/emacs}"
declare -r  TARBALL_DIR="${WORK_DIR}/tarballs"
declare -r  TARBALL_NAME="${VERSION}.tgz"
declare -r  TARBALL_FILE="${TARBALL_DIR}/${TARBALL_NAME}"
declare -r  TARBALL_URL="https://github.com/emacs-mirror/emacs/tarball/${VERSION}"
declare -r  TARBALL_EXTRACED_DIR="${WORK_DIR}/sources/${VERSION}"
# declare -r  PREFIX="${PREFIX:=${HOME}/.local}"

if is_macos; then
    declare -r makeCMD="gmake"
else
    declare -r makeCMD="make"
fi

function postprocess() {
    cd $ORIGIN
}
trap postprocess EXIT

function prepare_patches() {
    local PATCH_DIR=$1
    if [[ ! -d $PATCH_DIR ]]; then
        mkdir -p $PATCH_DIR
    fi

    cd $PATCH_DIR

    e_newline
    e_arrow "Downloading patches in ${PATCH_DIR}"

    for url in "${PATCH_URL[@]}"; do
        curl -LO $url
    done &&
    e_done "Patches downloaded." &&
    return 0 || exit 1
}


function apply_patches() {
    local FILE_DIR=$1
    local PATCH_DIR="${FILE_DIR}/patches"

    prepare_patches $PATCH_DIR &&
    local PATCH_FILES=($(ls -d ${PATCH_DIR}/*))

    e_newline
    e_arrow "Applying Patches..."
    cd $FILE_DIR
    for patch_file in "${PATCH_FILES[@]}"; do
        patch -f -p1 < $patch_file &&
        e_done "Apply patch: ${patch_file}" ||
        continue
    done &&
    return 0 || exit 1
}

function download_tarball() {
    [[ -d "$TARBALL_DIR" ]] || mkdir -p "$TARBALL_DIR"

    if [[ -d "$TARBALL_FILE" ]]; then
	      echo "${TARBALL_NAME} already exists locally, attempting to use."
    else
        e_newline
        e_arrow "Downloading emacs taball from ${TARBALL_URL}..."
	      curl -L $TARBALL_URL -o $TARBALL_FILE &&
	      e_done "Downloaded emacs tarball." &&
	      return 0 || exit 1
    fi
}

function extract_tarball() {
    [[ -d "$TARBALL_EXTRACED_DIR" ]] || mkdir -p "$TARBALL_EXTRACED_DIR"

    if [[ -f "$TARBALL_FILE" ]]; then
        e_newline
        e_arrow "Extracting tarball '${TARBALL_FILE}'..."
	      tar -xzf $TARBALL_FILE -C $TARBALL_EXTRACED_DIR &&
	      e_done "Extracted tarball." &&
	      return 0 || {
	          e_error "failed extracting tarball"
            exit 1
        }
    else
	      exit 1
    fi
}

function prepare_sources() {
    download_tarball &&
    extract_tarball &&
    return 0 || exit 1
}


function build() {
    if is_macos; then
        configureFlags=(
            "--with-modules"
            "--with-json"
            "--with-native-compilation=aot"
            "--with-imagemagick"
            "--with-xml2"
	          "--with-xwidgets"
            "--with-tree-sitter"
            "--with-ns"
            "--enable-mac-app=yes"
        )
    elif is_manjaro; then
        configureFlags=(
            "--with-modules"
            "--with-json"
            "--with-native-compilation=aot"
            "--with-xwidgets"
            "--with-pgtk"
	          "--without-x"
	          "--with-libxml2=/usr/bin/xml2-config"
            "--with-tree-sitter"
        )
    fi

    configureCMD="./configure"
    for cmd in "${configureFlags[@]}"; do
        configureCMD+=" $cmd"
    done

    # Pre-process
    if [[ -z "${SOURCE_DIR:-}" ]]; then
        prepare_sources &&
            SOURCE_DIR="$(ls -Artd ${TARBALL_EXTRACED_DIR}/* | tail -1)" || exit 1
    else
        cd $SOURCE_DIR &&
        eval "${makeCMD} extraclean" || exit 1
    fi &&

    if is_macos; then
        apply_patches $SOURCE_DIR || exit 1
    fi &&

    # Main-process
    cd $SOURCE_DIR &&
    ./autogen.sh &&
    eval $configureCMD &&
    eval "${makeCMD} --jobs=${NPROC} bootstrap" &&
    eval "${makeCMD} install" &&
    e_newline && e_done "Emacs Build processes are completed!" || exit 1

    if is_macos; then
        cp "${HOME}/.local/etc/helper/emacs-cli.bash" "${SOURCE_DIR}/nextstep/Emacs.app/Contents/MacOS/bin/emacs"
        chmod +x "${SOURCE_DIR}/nextstep/Emacs.app/Contents/MacOS/bin/emacs"

        cd ${SOURCE_DIR}/nextstep/
        codesign --force --deep -s - Emacs.app

        echo "Finally, recommend following steps:"
        echo "  mv ${SOURCE_DIR}/nextstep/Emacs.app /Applications/"
        echo "  sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacs ${HOME}/.local/bin/emacs"
        echo "  sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ${HOME}/.local/bin/emacsclient"
        echo ""
        echo "If necessary, execute following steps before create symlinks:"
        echo "  sudo unlink ${HOME}/.local/bin/emacs"
        echo "  sudo unlink ${HOME}/.local/bin/emacsclient"
    fi &&
    return 0 || exit 1
}

trap "e_error 'terminated'; exit 1" INT ERR

if [[ -e /opt/homebrew/ ]]; then
    if [[ -z ${LIBRARY_PATH:-} ]]; then
        export LIBRARY_PATH=/opt/homebrew/lib/
    else
        export LIBRARY_PATH=${LIBRARY_PATH}:/opt/homebrew/lib/
    fi

    if [[ -z ${CPATH:-} ]]; then
        export CPATH=/opt/homebrew/include/
    else
        export CPATH=${CPATH}:/opt/homebrew/include/
    fi
fi

build
