#!/bin/bash

# To indent and remove tabs, surround the piece of code with:
#   %% @indent-begin
#   %% @indent-end
#
# Then run:
#   make indent
#
# Please note this script only indents the first occurrence.

FILES=$(git grep --name-only @indent-begin src/)

for FILENAME in $FILES; do
    echo "==> Indenting $FILENAME..."
    emacs -batch $FILENAME \
       -f "erlang-mode" \
        --eval "(goto-char (point-min))" \
        --eval "(re-search-forward \"@indent-begin\" nil t)" \
        --eval "(setq begin (line-beginning-position))" \
        --eval "(re-search-forward \"@indent-end\" nil t)" \
        --eval "(setq end (line-beginning-position))" \
        --eval "(erlang-indent-region begin end)" \
        --eval "(untabify begin end)" \
       -f "delete-trailing-whitespace" \
       -f "save-buffer"
done
