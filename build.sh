#!/bin/bash

set -e

export OPAMYES=1

NCPU="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
OCAML_VERSION="5.1.0"
CLAML_OPAM_SWITCH=claml-"$OCAML_VERSION+flambda"

USER_OPAM_SWITCH=no

function usage() {
  echo "Usage: $0 [options]"
  echo
  echo " options:"
  echo "   -h,--help             show this message"
  echo "   --user-opam-switch    use the current opam switch to install haechi (default: $OPAM_SWITCH)"
  echo
  echo " examples:"
  echo "    $0                     # build haechi with default options"
  echo "    $0 --user-opam-switch  # build haechi in the current opam switch (e.g., for Github CI)"
}

while [[ $# -gt 0 ]]; do
  opt_key="$1"
  case $opt_key in
    --user-opam-switch)
      USER_OPAM_SWITCH=yes
      shift
      continue
      ;;
  esac
done

function setup_opam() {
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
}

if [ "$USER_OPAM_SWITCH" == "no" ]; then
  setup_opam
fi

opam pin add claml . -n
opam install -j $NCPU claml --deps-only
opam install ocamlformat.0.26.1
opam pin remove claml
./bootstrap.sh
make
