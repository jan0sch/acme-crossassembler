#!/usr/bin/env bash

make -C src
st=$?

if [ "$st" -eq 0 ]; then
  echo "Created ACME executable."
  exit 0
else
  echo "Could not compile ACME crossassembler!"
  exit 1
fi

