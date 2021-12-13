#!/bin/sh

FILENAME=$1

if grep -q [ぁ-ん] $FILENAME; then
    TEXTLINTRC="${HOME}/.config/textlint/textlintrc_ja.json"
else
    TEXTLINTRC="${HOME}/.config/textlint/textlintrc_en.json"
fi

EXTENTION=${FILENAME##*.}

if [ $EXTENTION = "org" ]; then
    PLUGIN="--plugin org"
elif [ $EXTENTION = "tex" ]; then
    PLUGIN="--plugin latex2e"
else
    PLUGIN=""
fi

textlint --format unix --config ${TEXTLINTRC} ${PLUGIN} ${FILENAME}
