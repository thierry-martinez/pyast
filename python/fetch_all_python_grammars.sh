#!/bin/bash
set -ex
if [ ! -d cpython ]; then
  git clone https://github.com/python/cpython.git
else
  (cd cpython && git fetch)
fi
versions=($(cd cpython && git tag --sort v:refname | grep '^v[[:digit:].]\+$'))
versions+=(v3.11.0rc2)
last=""
syntax_file="cpython/Parser/Python.asdl"
for version in "${versions[@]}"; do
    ( cd cpython && git checkout "$version" )
    if [ -f "$syntax_file" ] && ( [ -z "$last" ] || ! diff "$last" "$syntax_file" ); then
        target="$version".asdl
        cp "$syntax_file" "$target"
        last="$target"
    fi
done
mv v3.11.0rc2.asdl v3.11.0.asdl
