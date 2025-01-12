#!/bin/sh

set -xe

scryer-prolog --no-add-history -f -g run_tests tests.pl </dev/null
