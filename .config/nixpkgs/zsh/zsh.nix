# This file is loaded from programs.zsh block in home.nix.
# Options are here:
# https://rycee.gitlab.io/home-manager/options.html#opt-programs.zsh.enable

# deeply inspired by
# https://github.com/ahmedelgabri/dotfiles/blob/40d2941e36f680dde1a7f736f00cf9636dfbc003/config/zsh.d/.zshrc
# https://github.com/xeres/dotfiles/blob/3c7ca493e12d25ccecb588e992f30b5325fa9889/dot_zshrc

{ config, pkgs, ... }:

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
    size = 10000;
    path = "${config.xdg.dataHome}/zsh/history";
  };
  
  shellAliases = {
    # file
    ls = "ls --color=auto --file-type";
    ll = "exa -l -g --icons";
    lla = "ll -a";
    dua = "/usr/bin/du -shc * | sort -h";
    
    # latex compile command
    en_latex = "latexmk -e \"$bibtex=q/bibtex/\" -pdf -pvc";
    ja_latex = "latexmk -pvc";

    # emacs
    e = "emacsclient";
    ed = "emacs -nw --daemon";
    en = "emacsclient -nw";
    ekill = "emacsclient -e \"(kill-emacs)\"";

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

    # powerlevel10k-instant-prompt
    if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
    fi
    '';

  initExtra="source $HOME/.dotfiles/bin/my-server-util.bash";
  
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

    # Shell {{{
      PURE_SYMBOLS=("λ" "ϟ" "▲" "∴" "→" "»" "৸" "◗")
      # Arrays in zsh starts from 1
      export PURE_PROMPT_SYMBOL="''${PURE_SYMBOLS[$RANDOM % ''${#PURE_SYMBOLS[@]} + 1]}"
      zstyle :prompt:pure:path color 240
      zstyle :prompt:pure:git:branch color blue
      zstyle :prompt:pure:git:dirty color red
      zstyle :prompt:pure:git:action color 005
      zstyle :prompt:pure:prompt:success color 003
    # }}}

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

    zinit wait lucid for \
          atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
              zdharma-continuum/fast-syntax-highlighting \
         blockf \
             zsh-users/zsh-completions \
         atload"!_zsh_autosuggest_start" \
            zsh-users/zsh-autosuggestions

    zinit wait lucid for \
        atclone"curl -sOL https://github.com/ohmyzsh/ohmyzsh/raw/master/plugins/tmux/tmux.extra.conf" \
            OMZP::tmux

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


    # peco の設定（リポジトリ間の移動を行う）
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

    # peco の設定 （履歴を検索する）
    function peco-history-selection() {
        BUFFER=`history -n 1 | tac | awk '!a[$0]++' | peco`
        CURSOR=$#BUFFER
        zle reset-prompt
    }

    zle -N peco-history-selection
    bindkey '^R' peco-history-selection
 '';
}
