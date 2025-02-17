#! /bin/sh

set -xe

EX=0
GOAL_WRITE='pure_write('${EX}')'
GOAL_READ='pure_read'

scryer-prolog --no-add-history -f -g ${GOAL_WRITE} -g halt example.pl
scryer-prolog --no-add-history -f -g ${GOAL_READ}  -g halt example.pl
hexdump -C a.out
