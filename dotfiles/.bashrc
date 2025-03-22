# shellcheck shell=bash

PS1="\u@\h \w> "

source "$HOME/.nomad/sh/aliases"

export EDITOR=vim
export NOMAD="$HOME/.nomad"
export NOMAD_PICKER=sk

# .bashenv-nix is created by my home-manager configuration on systems where
# that is in use; this allows us to make nix-specific adjustments to the
# environment.
if [ -s "$HOME/.bashenv-nix" ]; then
    source "$HOME/.bashenv-nix"
fi

if [ -s "$NOMAD/bash/git-widgets.bash" ]; then
    source "$NOMAD/bash/git-widgets.bash"
    bind -x '"\C-x\C-g": fuzzy-git-branch'
fi

if command -v fzf >/dev/null 2>&1; then
    eval "$(fzf --bash)"
fi
