#!/usr/bin/env bash

set -e

stack build --profile
stack exec --profile -- kanji-exe ../a.gif +RTS -p 
