#!/bin/sh

# To indent and remove tabs, surround the piece of code with:
#   %% @indent-begin
#   %% @indent-end
#
# Install Emacs and erlang-mode. For example in Debian:
#   apt-get install emacs erlang-mode
#
# Then run:
#   make indent
#
# Please note this script only indents the first occurrence per file

FILES=$(git grep --name-only @indent-begin include/ src/)
LOG=${1:-/tmp/ejabberd-format.log}
EMACS=${2:-emacs}

if [ ! "$EMACS" ] || [ ! -x "$EMACS" ]
then
   echo "==> Cannot indent source code because Emacs is not installed"
   exit 1
fi

for FILENAME in $FILES; do
    echo "==> Indenting $FILENAME..." >>$LOG
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
       -f "save-buffer" >>$LOG 2>&1
done

grep -q 'Error' $LOG \
    && cat $LOG
grep -q 'Error: void-function (erlang-mode)' $LOG \
    && echo \
    && echo "==> Maybe you need to install erlang-mode system package" \
    && exit 1
rm $LOG
