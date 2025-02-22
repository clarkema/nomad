autoload -U is-at-least

#=======================================================================
# Basic options                                                      {{{
#=======================================================================

setopt autocd
setopt auto_pushd
setopt pushd_ignore_dups
setopt no_beep
setopt complete_in_word
setopt extended_glob

# }}} END basic options

setopt share_history
setopt hist_ignore_dups
is-at-least 4.3.11 && setopt hist_lex_words
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000

REPORTTIME=5

export NOMAD_PICKER="sk"

#=======================================================================
# Keybindings                                                        {{{
#=======================================================================

autoload -U edit-command-line

bindkey -v
bindkey '^R'  history-incremental-search-backward
bindkey '^A'  beginning-of-line
bindkey '^E'  end-of-line
bindkey '^L'  clear-screen
bindkey '^U'  kill-whole-line
bindkey '^Y'  yank
bindkey '^[.' insert-last-word
bindkey '^[q' push-line
bindkey '^G'  ft-zshexit
bindkey '^P'  up-line-or-history
bindkey '^N'  down-line-or-history
bindkey '^B'  backward-char
bindkey '^F'  forward-char
bindkey -M vicmd "ga" what-cursor-position

test -f "$NOMAD/zsh/git-widgets.zsh" && . "$NOMAD/zsh/git-widgets.zsh"
bindkey '^x^g' fuzzy-git-branch

for name in key-bindings.zsh completion.zsh; do
    file="$HOME/.nix-profile/share/fzf/$name"
    [ -s $file ] && source $file
done

function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/[N]}/(main|viins)/[I]}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

zle_use_ctrl_d="yes"
if [[ ${zle_use_ctrl_d} == 'yes' ]]; then
    setopt ignore_eof
    bindkey -M viins '^D' ft-vi-cmd
    bindkey -M vicmd '^D' ft-vi-cmd-cmd
    bindkey -r '^['
fi

# Since I want to bind `vi-cmd-mode' to Ctrl-D (which is what I'm doing in
# vim and emacs-viper, too) I need to wrap this widget into a user-widget,
# because only those have an effect with empty command buffers and bindings
# to the key, which sends `EOF'. This also needs the ignore_eof option set.
function ft-vi-cmd() {
    #ft_zle_state[overwrite]=no
    zle vi-cmd-mode
}
zle -N ft-vi-cmd

function ft-vi-cmd-cmd() {
    zle -M 'Use `:q<RET>'\'' to exit the shell.'
}
zle -N ft-vi-cmd-cmd

# This setup may change the `ESC' keybinding to `C-d'. That defeats the
# possibility to exit zsh by pressing `C-d' (which usually sends EOF).
# With this widget, you can type `:q<RET>' to exit the shell from vicmd.
function ft-zshexit {
    [[ -o hist_ignore_space ]] && BUFFER=' '
    BUFFER="${BUFFER}exit"
    zle .accept-line
}
zle -N ft-zshexit

# }}} END keybindings

#=======================================================================
# Per-directory profiles                                             {{{
#=======================================================================

if is-at-least 4.3.3 ; then

# chpwd_profiles(): Directory Profiles, Quickstart:
#
# In .zshrc.local:
#
#   zstyle ':chpwd:profiles:/usr/src/grml(|/|/*)'   profile grml
#   zstyle ':chpwd:profiles:/usr/src/debian(|/|/*)' profile debian
#   chpwd_profiles
#
# For details see the `grmlzshrc.5' manual page.
function chpwd_profiles() {
    local profile context
    local -i reexecute

    context=":chpwd:profiles:$PWD"
    zstyle -s "$context" profile profile || profile='default'
    zstyle -T "$context" re-execute && reexecute=1 || reexecute=0

    if (( ${+parameters[CHPWD_PROFILE]} == 0 )); then
        typeset -g CHPWD_PROFILE
        local CHPWD_PROFILES_INIT=1
        (( ${+functions[chpwd_profiles_init]} )) && chpwd_profiles_init
    elif [[ $profile != $CHPWD_PROFILE ]]; then
        (( ${+functions[chpwd_leave_profile_$CHPWD_PROFILE]} )) \
            && chpwd_leave_profile_${CHPWD_PROFILE}
    fi
    if (( reexecute )) || [[ $profile != $CHPWD_PROFILE ]]; then
        (( ${+functions[chpwd_profile_$profile]} )) && chpwd_profile_${profile}
    fi

    CHPWD_PROFILE="${profile}"
    return 0
}

chpwd_functions=( ${chpwd_functions} chpwd_profiles )

fi # is433

# }}} END per-directory profiles

#
# Custom function definitions, including custom completions.
#
fpath=( "$NOMAD/zsh/functions" "$NOMAD/local/share/zsh/functions" "$HOME/.nix-profile/share/zsh/functions" "${fpath[@]}" )
autoload -U _tm

# Programmable completion
autoload -U compinit; compinit

if ! (is-at-least 4.3.9); then
    autoload -U colors
    colors
fi

[[ -f $HOME/.nomad/sh/shared ]] && . $HOME/.nomad/sh/shared

#=======================================================================
# Git-ified prompt                                                   {{{
#=======================================================================

default_prompt="%h %n@%m:%~ ${vcs_info_msg_0_}%(!.#.>) "
precmd() {
    local git_branch=$(parse_git_branch)
    local gitcolor gitcolor_new gitcolor_old reset
    reset=%f

    if [[ -n "$git_branch" ]]; then
        git_clean_p
        if [[ $? == 0 ]]; then
            if [[ "$git_branch" == "master" ]]; then
                gitcolor=%F{yellow}
                gitcolor_old=${fg[yellow]}
            else
                gitcolor=%F{green}
                gitcolor_old=${fg[green]}
            fi
        else
            gitcolor=%F{red}
            gitcolor_old=${fg[red]}
        fi

        if ! (is-at-least 4.3.9); then
            gitcolor=$gitcolor_old
            reset=$reset_color
        fi

        if [ -n "$IN_NIX_SHELL" ]; then
            nix="[nix] "
        else
            nix=""
        fi

        PROMPT="%h ${nix}$(path_within_git_repo):%{$gitcolor%}$git_branch%{$reset%}%(!.#.>) "
    else
        PROMPT=$default_prompt
    fi
}

function parse_git_branch ()
{
    #git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
    local ref=$(git symbolic-ref HEAD 2> /dev/null) || return 1
    echo ${ref#refs/heads/}
}

function path_within_git_repo ()
{
    local repo_base=${$( git rev-parse --git-dir ):A}

    local repo_parent=${${repo_base%.git}:h}
    print -r ${PWD##$repo_parent/}
}

# Exit value of 0 if the repo is clean, 1 otherwise
function git_clean_p ()
{
    git status 2> /dev/null | grep -F 'working tree clean' > /dev/null
}

# }}}

#=======================================================================
# Source shared aliases                                              {{{
#=======================================================================

[ -f $NOMAD/sh/aliases ] && . $NOMAD/sh/aliases

# }}} END source shared aliases

#=======================================================================
# Zsh-specific aliases                                               {{{
# These make use of features specific to zsh.  More general aliases
# are in sh/aliases.
#=======================================================================

alias doch='sudo $(fc -ln -1)' # not much shorter than 'sudo !!'
                               # but more satisfying!
alias ez='exec zsh'
alias -g L='| less'
alias -g G='| grep'

# }}}

function rtm ()
{
    local server=$1
    shift 1

    ssh -t "$server" "SHELL=/usr/bin/zsh ~/bin/tm $@"
}

#=======================================================================
# OS-specifc aliases                                                 {{{
#=======================================================================

case $(uname) in
    Darwin)
        alias ls='ls -GF'
        ;;
esac

# }}}

export EDITOR=vim

export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache

export OOO_FORCE_DESKTOP=gnome

export START_PATH=$PATH

function ncd ()
{
    mkdir -p "$1" && cd "$1"
}

function source_environment_specific_files ()
{
    local fqdn components domainname hostname
    local domainfile hostfile

    case $(uname -s) in
        "Linux")
            fqdn=$(hostname -f)
            ;;
        "Darwin"|"FreeBSD"|"NetBSD"|*)
            fqdn=$(hostname)
            ;;
    esac
    components=(${(s:.:)fqdn})
    hostname=$components[1]

    if [[ ${#components} -ge 2 ]]; then
        domainname=${(j:.:)components[2,-1]}
    else
        # If we've got here we have only an unqualified hostname in $HOST
        # instead of an FQDN.  This is broken but does happen; in
        # particular on sdf.org's NetBSD machines.
        if [[ "$(uname -s)" == "NetBSD" && "$(domainname)" == "SDF" ]]; then
        domainname="sdf.org"
        fi
    fi

    domainfile="$HOME/.zsh/domains/$domainname/all"
    hostfile="$HOME/.zsh/domains/$domainname/$hostname"

    for f in $domainfile $hostfile; do
        if [[ -f $f  ]]; then
            source $f
        fi
    done
}
source_environment_specific_files

function tmx ()
{
   tmux resize-pane -x ${1-80}
}

function tmxh ()
{
    local cols=$(tmux list-windows -F "#{window_width}" | head -n 1)
    tmux resize-pane -x $(( cols / 2 ))
}

function tmy ()
{
    tmux resize-pane -y ${1-25}
}

function tmyh ()
{
    local rows=$(tmux list-windows -F "#{window_height}" | head -n 1)
    tmux resize-pane -y $(( rows / 2 ))
}

function rgc ()
{
    local rg_args="${*[1,$# - 1]}" # All but last arg
    local query="${*[-1]}" # Last arg

    pkd=$(sk --ansi -i -c "rg $rg_args -n --color always '{}'" --preview "preview.sh {}" --cmd-query "$query" | awk -F: '{print $1}')
    echo $pkd
}
alias vp='vim "$pkd"'
alias nvp='nvim "$pkd"'
alias enp='emacsclient -n "$pkd"'

#=====================================================================
# Pull in secret configuration settings from ~/.secrets
if [[ -f ~/.secrets ]] then
    source ~/.secrets
fi

if which rbenv > /dev/null 2>&1; then eval "$(rbenv init -)"; fi

[ -s $NOMAD/breeze/scm_breeze.sh ] && source $NOMAD/breeze/scm_breeze.sh

# OCaml package configuration
[[ ! -r $HOME/.opam/opam-init/init.zsh ]] || source $HOME/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

if command -v zoxide > /dev/null; then
    eval "$(zoxide init zsh --cmd j)"
fi
