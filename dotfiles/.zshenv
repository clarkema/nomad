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
    $HOME/.asdf/shims
    $HOME/.cabal/bin
    $HOME/.cargo/bin
    $HOME/.raku/bin
    $HOME/.mix/escripts
    $HOME/.software/rakudo/bin
    $HOME/.software/rakudo/share/perl6/site/bin
    $HOME/.software/rakudo/share/perl6/vendor/bin
    $HOME/.software/rakudo/share/perl6/core/bin
    $HOME/.npm-packages/bin
    $HOME/.gem/ruby/3.0.0/bin
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

# Set up nix if it's installed.  We have to deal with three cases.
if [ -s "/etc/profile.d/nix.sh" ]; then
    # Multi-user Linux.  In this case the profile should already have been
    # sourced, so we don't need to do anything.
    :
elif [ -s "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    # Single-user Linux.  Our system-level profile doen't know anything about
    # this so we need to source it.
    source "$HOME/.nix-profile/etc/profile.d/nix.sh"
elif [ -s "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]; then
    # Multi-user macOS.  Again, the nix installer should have added this to
    # /etc/zshrc, so manual sourcing should be required.
    :
fi

# .zshenv-nix is created by my home-manager configuration on systems where that
# is in use; this allows us to make nix-specific adjustments to the environment.
if [ -e "$HOME/.zshenv-nix" ]; then
    source "$HOME/.zshenv-nix"
fi
