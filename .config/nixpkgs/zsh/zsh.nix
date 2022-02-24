# This file is loaded from programs.zsh block in home.nix.
# Options are here:
# https://rycee.gitlab.io/home-manager/options.html#opt-programs.zsh.enable

# deeply inspired by
# https://github.com/ahmedelgabri/dotfiles/blob/40d2941e36f680dde1a7f736f00cf9636dfbc003/config/zsh.d/.zshrc
# https://github.com/xeres/dotfiles/blob/3c7ca493e12d25ccecb588e992f30b5325fa9889/dot_zshrc
# https://zenn.dev/kis9a/scraps/02f3ec438d93d1

{ config }:

{
  enable = true;
  enableAutosuggestions = false;
  enableCompletion = false;
  enableSyntaxHighlighting = false;
  autocd = true;
  defaultKeymap = "emacs";

  history = {
    expireDuplicatesFirst = true;
    ignoreDups = true;
    ignoreSpace = false;
    share = true;
    size = 10000;
    path = "${config.xdg.dataHome}/zsh/history";
  };

  shellGlobalAliases = {
    G = "| grep";
  };
  
  shellAliases = {
    sudo = "sudo ";
    rm = "~/src/github.com/naoking158/rm-alternative/rm-alternative.bash";
    
    # move
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    
    # file
    cat = "bat";
    diff = "diff --color=auto";
    dua = "/usr/bin/du -shc * | sort -h";
    fd = "fd --color=auto --full-path --no-ignore --hidden --exclude \".git\"";
    grep = "grep --color=auto";
    ls = "exa --icons";
    la = "exa -a";
    ll = "exa -l -g --icons";
    lla = "ll -a";
    rg = "rg --color=auto --no-ignore --hidden --glob=\"!.git\" --line-number";
    tgz = "f() { env COPYFILE_DISABLE=1 tar zcvf $1 --exclude=\".DS_Store\" \${@:2}; unset -f f; }; f";
    tree = "exa --tree --level 3 -a --ignore-glob \"node_modules|.git|.cache\" --icons";
    

    # conda
    c = "conda";
    ca = "c activate";
    ce = "c env";
    ci = "c install";
    cs = "c search";
    
    # emacs
    e = "emacsclient";
    ed = "emacs -nw --daemon";
    eq = "emacs -q";
    eql = "eq -l";
    enw = "emacsclient -nw";
    ekill = "emacsclient -e \"(kill-emacs)\"";

    # git
    g = "git";
    gc = "g commit -m";
    gs = "g status";
    gas = "g add -A && gs";

    # latex compile command
    en_latex = "latexmk -e \"$bibtex=q/bibtex/\" -pdf -pvc";
    ja_latex = "latexmk -pvc";

    # nix
    ne-search="nix-env -qa";
    nc-list="nix-channel --list";
    nc-update="nix-channel --update";
    
    # ssh
    _kingkong = "ssh kingkong";
    __kingkong = "ssh _kingkong";
    _zeus = "ssh zeus";
    __zeus = "ssh _zeus";
    _gorilla1 = "ssh gorilla1";
    __gorilla1 = "ssh _gorilla1";
    _gorilla2 = "ssh gorilla2";
    __gorilla2 = "ssh _gorilla2";
    _gorilla3 = "ssh gorilla3";
    __gorilla3 = "ssh _gorilla3";
    _gorilla4 = "ssh gorilla4";
    __gorilla4 = "ssh _gorilla4";
    _gorilla5 = "ssh gorilla5";
    __gorilla5 = "ssh _gorilla5";
    _mdl = "ssh _mdl";
  };

  initExtraFirst = ''
    # Set global environment variables
    [[ -f "''${HOME}/.profile" ]] && source "''${HOME}/.profile"
    '';

  initExtra = ''
    source $HOME/.dotfiles/bin/my-server-util.bash

    # powerlevel10k-instant-prompt
    if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
    fi

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
    '';
  
  initExtraBeforeCompInit = ''
    declare -A ZINIT

    ##############################################################
    # ZINIT https://github.com/zdharma-continuum/zinit
    ##############################################################
    ZINIT[HOME_DIR]="$XDG_CACHE_HOME/zsh/zinit"
    ZINIT[BIN_DIR]="$ZINIT[HOME_DIR]/bin"
    ZINIT[PLUGINS_DIR]="$ZINIT[HOME_DIR]/plugins"
    ZINIT[ZCOMPDUMP_PATH]="$XDG_CACHE_HOME/zsh/zcompdump"
    # export ZINIT[OPTIMIZE_OUT_DISK_ACCESSES]=1
    export ZPFX="$ZINIT[HOME_DIR]/polaris"

    local __ZINIT="$ZINIT[BIN_DIR]/zinit.zsh"

    if [[ ! -f "$__ZINIT" ]]; then
      if (( $+commands[git] )); then
        git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT[BIN_DIR]"
      else
        echo 'git not found' >&2
        exit 1
      fi
    fi

    source "$__ZINIT"
    autoload -Uz _zinit
    (( ''${+_comps} )) && _comps[zinit]=_zinit

    # Utilities & enhancements {{{
      zinit ice wait lucid
      zinit light https://github.com/zsh-users/zsh-history-substring-search
      # bind UP and DOWN keys
      bindkey "''${terminfo[kcuu1]}" history-substring-search-up
      bindkey "''${terminfo[kcud1]}" history-substring-search-down

      # bind UP and DOWN arrow keys (compatibility fallback)
      bindkey '^[[A' history-substring-search-up
      bindkey '^[[B' history-substring-search-down
    # }}}

    # builtins
    function chpwd() {
        if [[ $(pwd) != $HOME ]]; then;
            exa -a --icons
        fi
    }

    # cdr
    if [[ -n $(echo ''${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ''${^fpath}/cdr(N)) ]]; then
        autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
        add-zsh-hook chpwd chpwd_recent_dirs
        zstyle ':completion:*' recent-dirs-insert both
        zstyle ':chpwd:*' recent-dirs-default true
        zstyle ':chpwd:*' recent-dirs-max 1000
        # zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/chpwd-recent-dirs"
    fi

    # zinit plugins
    zinit wait lucid for \
         atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
             zdharma-continuum/fast-syntax-highlighting \
         atload"!_zsh_autosuggest_start" \
             zsh-users/zsh-autosuggestions \
         blockf \
             zsh-users/zsh-completions \
         as"blockf; completion; snippet" \
             https://github.com/esc/conda-zsh-completion/blob/master/_conda \
         as"blockf; completion; snippet" \
             https://github.com/spwhitt/nix-zsh-completions/blob/master/_nix-common-options

    zinit wait lucid for \
        atclone"curl -sOL https://github.com/ohmyzsh/ohmyzsh/raw/master/plugins/tmux/tmux.extra.conf" \
            OMZP::tmux

    zinit ice pick"bd.zsh"; zinit light Tarrasch/zsh-bd

    zinit ice atclone"dircolors -b LS_COLORS > clrs.zsh" \
        atpull'%atclone' pick"clrs.zsh" nocompile'!' \
        atload'zstyle ":completion:*" list-colors “''${(s.:.)LS_COLORS}”'
    zinit light trapd00r/LS_COLORS

    zinit ice depth=1; zinit light romkatv/powerlevel10k
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

    zinit lucid from:'gh-r' \
            as:'program' \
            pick:'delta*/delta' \
            light-mode \
            for @dandavison/delta

    zinit ice from"gh-r" as"command"
    zinit light junegunn/fzf
    zinit ice lucid as"command" id-as"junegunn/fzf-tmux" pick"bin/fzf-tmux"
    zinit light junegunn/fzf
    zinit ice lucid multisrc"shell/{completion,key-bindings}.zsh" id-as"junegunn/fzf_completions" pick"/dev/null"
    zinit light junegunn/fzf

    zinit ice from"gh-r" as"command" pick"*/ghq"
    zinit light x-motemen/ghq

    zinit light mollifier/anyframe
    bindkey '^x^b' anyframe-widget-cdr
    bindkey '^x^f' anyframe-widget-insert-filename
    bindkey '^x^g' anyframe-widget-cd-ghq-repository
    bindkey '^x^k' anyframe-widget-kill
    bindkey '^x^r' anyframe-widget-put-history
    zstyle ":anyframe:selector:" use fzf-tmux


    # move repositories with peco
    function peco-src () {
      local selected_dir=$(ghq list -p | peco --query "$LBUFFER")
      if [ -n "$selected_dir" ]; then
        BUFFER="cd ''${selected_dir}"
        zle accept-line
      fi
      zle clear-screen
    }
    zle -N peco-src
    bindkey '^o' peco-src

    # search history with peco
    function peco-history-selection() {
        BUFFER=`history -n 1 | tac | awk '!a[$0]++' | peco`
        CURSOR=$#BUFFER
        zle reset-prompt
    }

    zle -N peco-history-selection
    bindkey '^R' peco-history-selection

    # cdr with peco
    function peco-cdr () {
        local selected_dir="$(cdr -l | sed 's/^[0-9]\+ \+//' | peco --prompt="cdr >" --query "$LBUFFER")"
        if [ -n "$selected_dir" ]; then
            BUFFER="cd `echo $selected_dir | awk '{print$2}'`"
            CURSOR=$#BUFFER
            zle reset-prompt
        fi
    }
    zle -N peco-cdr
    bindkey '^[^R' peco-cdr

    # nix home-manager functions
    function home-update () {
        case "$SYSTEM" in
            "macos" ) home-manager switch -f $XDG_CONFIG_HOME/nixpkgs/macos.nix ;;
            * ) home-manager switch -f $XDG_CONFIG_HOME/nixpkgs/linux.nix ;;
        esac
     }

     function home-package () {
         if [[ $# = 0 ]]; then
            home-manager packages
        else
            home-manager packages | grep $@
        fi
    }
    '';
}
