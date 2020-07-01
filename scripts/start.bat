#!/bin/sh

set -e

elm-live src/Main.elm --open --start-page=index.html --dir=public -- --output=public/elm.js --debug
