#!/bin/bash

set -e

export OPAMYES=1

NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
OCAML_VERSION="4.13.1"
CLAML_OPAM_SWITCH=claml-"$OCAML_VERSION"
opam init --compiler=$OCAML_VERSION -j $NCPU --no-setup

switch_exists=no
for installed_switch in $(opam switch list --short); do
  if [[ "$installed_switch" == "$CLAML_OPAM_SWITCH" ]]; then
    switch_exists=yes
    break
  fi
done
if [ "$switch_exists" = "no" ]; then
  opam switch create $CLAML_OPAM_SWITCH $OCAML_VERSION
else
  opam switch $CLAML_OPAM_SWITCH
fi

eval $(SHELL=bash opam config env --switch=$CLAML_OPAM_SWITCH)
opam pin add claml . -y
opam pin remove claml
