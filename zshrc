autoload -U is-at-least

setopt autocd
setopt auto_pushd
setopt pushd_ignore_dups
setopt no_beep
#setopt auto_menu
setopt complete_in_word


setopt share_history
setopt hist_ignore_dups
is-at-least 4.3.11 && setopt hist_lex_words
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000

REPORTTIME=5

setopt extended_glob

bindkey -v
bindkey '^R'  history-incremental-search-backward
bindkey '^A'  beginning-of-line
bindkey '^E'  end-of-line
bindkey '^L'  clear-screen
bindkey '^U'  kill-whole-line
bindkey '^[.' insert-last-word
bindkey '^[q' push-line
bindkey '^G'  ft-zshexit
bindkey -M vicmd "ga" what-cursor-position

function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/[N]}/(main|viins)/[I]}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

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

#
# Custom function definitions, including custom completions.
#
fpath=( ~/.zsh/functions $fpath )
autoload -U _tm

# Programmable completion
autoload -U compinit; compinit

if ! (is-at-least 4.3.9); then
    autoload -U colors
    colors
fi

#=====================================================================
# Git-ified prompt
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

        PROMPT="%h $(path_within_git_repo):%{$gitcolor%}$git_branch%{$reset%}%(!.#.>) "
    else
        PROMPT=$default_prompt
    fi
}

PATH=$HOME/bin:$HOME/local/bin:/usr/local/bin:/usr/local/sbin:$PATH:$HOME/.cabal/bin
PATH=$PATH:/usr/local/texlive/2011/bin/universal-darwin

function parse_git_branch ()
{
    #git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
    local ref=$(git symbolic-ref HEAD 2> /dev/null) || return 1
    echo ${ref#refs/heads/}
}

function path_within_git_repo ()
{
    local repo_base=$( git rev-parse --git-dir )

    if [[ $repo_base == '.' ]]; then
        # We're in the base repo directory, so git rev-parse has just
        # given us '.'
        repo_base=$PWD
    elif [[ $repo_base == '.git' ]]; then
        repo_base=$PWD
    fi

    local repo_parent=${${repo_base%%.git}:h}
    print -r ${PWD##$repo_parent/}
}

# Exit value of 0 if the repo is clean, 1 otherwise
function git_clean_p ()
{
    git status 2> /dev/null | fgrep 'working directory clean' > /dev/null
}

export PATH

#=====================================================================
# Aliases
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias doch='sudo $(fc -ln -1)' # not much shorter than 'sudo !!'
                               # but more satisfying!
alias ez='exec zsh'
alias -g L='| less'
alias -g G='| grep'
alias v='vim'

alias g=git
alias g+='git add'
alias gco='git checkout'
alias gci='git commit'
alias gcp='git cherry-pick'
alias gd='git diff'
alias gdc='git diff --cached'
alias gf='git fetch'
alias gl='git log'
alias glg='git log --graph --format="%C(yellow)%h%Creset [%aN]%Cgreen%d%Creset% s" --all'
alias gp='git push'
alias gpo='git push origin'
alias gpfo='git push -f origin'
alias gr='git rebase'
alias gri='git rebase -i'
alias gs='git status'
alias gs='git status'
alias gst='git stash'
alias gsp='git stash pop'
alias gsm='git submodule'

function rtm ()
{
    local server=$1
    shift 1

    ssh -t "$server" "SHELL=/usr/bin/zsh ~/bin/tm $@"
}

#---------------------------------------------------------------------
# OS-specifc aliases
case $(uname) in
    Darwin)
        alias ls='ls -GF'
        ;;
esac

export EDITOR=vim

export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache

export OOO_FORCE_DESKTOP=gnome

export START_PATH=$PATH

function ncd ()
{
    mkdir -p "$1" && cd "$1"
}

if [[ -f $HOME/.fasd ]]; then
  # Initialize fasd (https://github.com/clvv/fasd)
  eval "$(fasd --init auto)"

  # aliases

  # jump to recently used items
  alias a='fasd -a' # any
  alias s='fasd -s' # show / search / select
  alias d='fasd -d' # directory
  alias f='fasd -f' # file
  alias z='fasd_cd -d' # cd, same functionality as j in autojump
  #alias v='f -e vim' # quick opening files with vim
fi

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
        if [[ "$(domainname)" == "SDF" && "$(uname -s)" == "NetBSD" ]]; then
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

#=====================================================================
# Pull in secret configuration settings from ~/.secrets
if [[ -f ~/.secrets ]] then
    source ~/.secrets
fi
