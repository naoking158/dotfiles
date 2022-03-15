#!/bin/bash


set -Ceu

readonly ORIGIN=$(pwd)
readonly WORK_DIR="${HOME}/src/emacs"
readonly TARBALLS_DIR="${WORK_DIR}/tarballs"
readonly SOURCE_DIR="${WORK_DIR}/sources"
# readonly WORK_DIR="${HOME}/src/github.com/emacs-mirror/emacs"

PATCH_DIR="${WORK_DIR}/patches"
PATCH_URL=(
    "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-28/fix-window-role.patch"
    "https://github.com/d12frosted/homebrew-emacs-plus/raw/master/patches/emacs-28/system-appearance.patch"
)

function e_error() {
    printf " \033[31m%s\033[m\n" "✖ $*" 1>&2
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

function postprocess() {
    $(cd $ORIGIN)
}
trap postprocess EXIT

function prepare_patches() {
    if [[ ! -d $PATCH_DIR ]]; then
        mkdir -p $PATCH_DIR
    fi

    e_header "Downloading patches in ${PATCH_DIR}"
    cd $PATCH_DIR
    for url in "${PATCH_URL[@]}"; do
        curl -LO $url
    done &&
    e_done "Patches downloaded." &&
    return 0 || exit 1
}


function apply_patches() {
    PATCH_FILES=($(ls $PATCH_DIR))
    
    e_header "Applying Patches..."
    cd $WORK_DIR
    for patch_file in "${PATCH_FILES[@]}"; do
        patch -p1 < "${PATCH_DIR}/${patch_file}" &&
        e_done "Apply patch: ${patch_file}"
    done &&
    return 0 || exit 1   
}

function download_tarball() {
    [[ -d $TARBALLS_DIR ]] || mkdir -p $TARBALLS_DIR
    local FILENAME="emacs-28.tgz"
    local TARGET="${TARBALLS_DIR}/${FILENAME}"

    e_important "Downloading emacs taball..."

    if [[ -d $TARGET ]]; then
	echo "${FILENAME} already exists locally, attempting to use."
    else
	local URL="https://github.com/emacs-mirror/emacs/tarball/emacs-28"
	curl -L $URL -o $TARGET &&
	    e_done "Downloaded emacs tarball." &&
	    return 0 || exit 1
    fi

}

function extract_tarball() {
    local FILENAME="emacs-28.tgz"
    local TARGET="${TARBALLS_DIR}/${FILENAME}"
    [[ -d $SOURCE_DIR ]] || mkdir -p $SOURCE_DIR

    e_important "Extracting tarball..."
    
    if [[ -f $TARGET ]]; then
	tar -xzf $TARGET -C $SOURCE_DIR &&
	    e_done "Extracted tarball..." &&
	    return 0  ||
		{
		    e_error "failed extracting tarball"
		    exit 1
		}
    else
	exit 1
    fi
}

function build() {
    configureFlags=(
        "--with-modules"
        "--with-json"
        "--with-native-compilation"
        "--with-imagemagick"
        "--with-xml2"
	"--with-xwidgets"
	"--prefix=${HOME}/.local"
    )

    configureCMD="./configure"
    for cmd in "${configureFlags[@]}"; do
        configureCMD+=" $cmd"
    done

    if is_macos; then
        makeCMD="gmake"
    else
        makeCMD="make"
    fi

    # Pre-process
    download_tarball &&
    extract_tarball

    if is_macos; then
        prepare_patches &&
        apply_patches || exit 1
    fi

    cd "${SOURCE_DIR}/$(ls ${SOURCE_DIR})" &&
    ./autogen.sh &&
    eval $configureCMD &&
    eval "${makeCMD} clean" &&
    eval "${makeCMD} bootstrap" &&
    eval "${makeCMD} install" &&
    e_done "Emacs Build processes are completed!" || exit 1

    if is_macos; then
        cp "${HOME}/.dotfiles/etc/helper/emacs-cli.bash" "${WORK_DIR}/nextstep/Emacs.app/Contents/MacOS/bin/emacs"
        chmod +x "${WORK_DIR}/nextstep/Emacs.app/Contents/MacOS/bin/emacs"

        echo "Finally, recommend following steps:"
        echo "  mv ${WORK_DIR}/nextstep/Emacs.app /Applications/"
        echo "  sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacs /usr/local/bin/emacs"
        echo "  sudo ln -s /Applications/Emacs.app/Contents/MacOS/bin/emacsclient /usr/local/bin/emacsclient"
        echo ""
        echo "If necessary, execute following steps before create symlinks:"
        echo "  sudo unlink /usr/local/bin/emacs"
        echo "  sudo unlink /usr/local/bin/emacsclient"
    fi &&
    return 0 || exit 1
}


trap "e_error 'terminated'; exit 1" INT ERR
build
