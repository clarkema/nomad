# shellcheck shell=bash

# Bail out if we're not interactive
[[ $- != *i* ]] && return

PS1="\u@\h:\w > "

export EDITOR=vim
export NOMAD="$HOME/.nomad"
export NOMAD_PICKER=sk

source_if_exists() {
  [[ -s "$1" ]] && source "$1"
}

source_if_exists "$NOMAD/sh/aliases"
source_if_exists "$NOMAD/sh/funcs"

# .bashenv-nix is created by my home-manager configuration on systems where
# that is in use; this allows us to make nix-specific adjustments to the
# environment.
source_if_exists "$HOME/.bashenv-nix"

if source_if_exists "$NOMAD/bash/git-widgets.bash"; then
    bind -x '"\C-x\C-g": fuzzy-git-branch'
fi

if command -v fzf >/dev/null 2>&1; then
    eval "$(fzf --bash)"
fi

source_if_exists "$NOMAD/breeze/scm_breeze.sh"

unset -f source_if_exists
