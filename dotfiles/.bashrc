# shellcheck shell=bash

PS1="\u@\h \w> "

source "$HOME/.nomad/sh/aliases"

export EDITOR=vim

# .bashenv-nix is created by my home-manager configuration on systems where
# that is in use; this allows us to make nix-specific adjustments to the
# environment.
if [ -e "$HOME/.bashenv-nix" ]; then
    source "$HOME/.bashenv-nix"
fi
