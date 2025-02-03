#! /bin/sh

set -xe

EX=0
GOAL_WRITE='pure_write('${EX}')'
GOAL_READ='pure_read'

scryer-prolog --no-add-history -f -g ${GOAL_WRITE} example.pl </dev/null
scryer-prolog --no-add-history -f -g ${GOAL_READ} example.pl </dev/null
hexdump -C a.out
