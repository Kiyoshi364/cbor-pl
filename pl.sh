#!/bin/sh

set -xe

Goal='run_tests'

time scryer-prolog --no-add-history -f -g $Goal tests.pl </dev/null
