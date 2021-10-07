#!/bin/bash
set -ex
if [ ! -d cpython ]; then
  git clone https://github.com/python/cpython.git
else
  (cd cpython && git fetch)
fi
versions=($(ls -1 ../../python/*.asdl | sed -n 's/^.*\(v[[:digit:].]\+\)[.].*$/\1/p'))
syntax_file="cpython/Parser/Python.asdl"
for version in "${versions[@]}"; do
    ( cd cpython && git checkout "$version" )
    if ! conda run --name "$version" true; then
        if ! conda create --yes --name "$version" python="${version#v}"; then
            continue
        fi
    fi
    conda run --name "$version" dune exec \
        parse_with_all_versions/parse_with_all_versions.exe \
        cpython/Lib/argparse.py
done
