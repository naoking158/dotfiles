
function _rm
    set TRASH $HOME/.myTrash

    if test -e $TRASH/$argv
        mv $TRASH/$argv $TRASH/$argv-(date +%Y%m%d%I%M%S)
    end
    mv -i $argv $HOME/.myTrash/
end

function rm
    argparse -n mycmdname f/force -- $argv
    or return

    if set -lq _flag_f
        /bin/rm -rf $argv
    else
        set TRASH $HOME/.myTrash
        if ! test -e $TRASH
            mkdir $TRASH
        end

        for arg in $argv
            _rm $arg
        end
        echo "They are moved to $HOME/.myTrash"
    end
end

function en_latex
    command latexmk -e "$bibtex=q/bibtex/" -pdf -pvc $argv
end

function ja_latex
    command latexmk -pvc $argv
end

function reformatpdftoeps
    find . -name "*.pdf" -print0 | xargs -0 -I '{}' pdftops -f 1 -l 1 -eps '{}' '{}.eps'
end

function renamepdfeps
    find . -name "*.pdf.eps" -print0 | xargs -0 -I '{}' rename 's/(.*\.)pdf.eps/$1eps/' '{}'
end

function replace --argument-names 'before' 'after'
    find . -name "*$before*" | xargs rename -s $before $after
end

function du
    command /usr/bin/du -shc * | sort -h
end

# # check existence and initialize of zoxide command 
# if type -q zoxide
#     zoxide init fish | source
# end
