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
    $HOME/.cargo/bin
    $HOME/.raku/bin
    $HOME/.mix/escripts
    $HOME/.software/rakudo-star-2020.10/bin
    $HOME/.software/rakudo-star-2020.10/share/perl6/site/bin
    $HOME/.software/rakudo-star-2020.10/share/perl6/vendor/bin
    $HOME/.software/rakudo-star-2020.10/share/perl6/core/bin
    $HOME/.nix-profile/bin
    $HOME/.nix-profile/sbin
    $HOME/.npm-packages/bin
    /usr/local/texlive/2019/bin/x86_64-darwin
    /usr/local/sbin
    /usr/local/bin
    /opt/homebrew/sbin
    /opt/homebrew/bin
    $path
    /usr/sbin
    /usr/bin
    /sbin
    /bin
    $HOME/.rvm/bin
)

# Use only those elements of the path array that exist.
path=($^path(N-/))

source $NOMAD/sh/env
