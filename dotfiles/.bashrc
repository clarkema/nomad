# shellcheck shell=bash

# Bail out if we're not interactive
[[ $- != *i* ]] && return

PS1="\u@\h:\w > "

export EDITOR=vim
export NOMAD="$HOME/.nomad"
export NOMAD_PICKER=sk

source_if_exists() {
  # Stop shellcheck complaining about a non-constant source -- that's the whole
  # point of this function
  # shellcheck source=/dev/null
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

# For symmetry with my ZSH config
bind '"\C-g": "exit\n"'

if command -v fzf >/dev/null 2>&1; then
    eval "$(fzf --bash)"
fi

if command -v zoxide > /dev/null; then
    eval "$(zoxide init bash --cmd j)"
fi

if [ "$(hostname)" == "muninn" ]; then
    alias nv='NVIM_APPNAME=nvim_eink nvim'
fi

source_if_exists "$NOMAD/breeze/scm_breeze.sh"

unset -f source_if_exists
