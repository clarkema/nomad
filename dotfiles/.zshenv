########################################################################
# Nomad
#
# dotfiles/.zshenv: sourced on all invocations of the shell.
# Here we set search paths and other environment variables that are
# required regardless of how the shell being used.
########################################################################

export NOMAD=$HOME/.nomad

typeset -U path
path=(
    $HOME/bin
    $HOME/.nomad/bin
    $HOME/.nomad/local/bin
    $HOME/.local/bin
    $HOME/.cabal/bin
    /usr/local/texlive/2012/bin/universal-darwin
    /usr/local/sbin
    /usr/local/bin
    $path
    /usr/sbin
    /usr/bin
    /sbin
    /bin
)

source $NOMAD/sh/env
