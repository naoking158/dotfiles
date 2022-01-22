
[[ -f $HOME/.profile ]] && . "$HOME/.profile"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$(\"$MINICONDA/bin/conda\" 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "$MINICONDA/etc/profile.d/conda.sh" ]; then
        . "$MINICONDA/etc/profile.d/conda.sh"
    else
        export PATH="$MINICONDA/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

case $- in
    *i*) ;;
    *) return;;
esac

# Fish Shell
# if [ -z "$FISH_VERSION" ]; then
#     command -v fish > /dev/null 2>&1 && exec fish
# fi