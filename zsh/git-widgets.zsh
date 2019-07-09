# zsh widgets for git commands -- part of Nomad.
# https://github.com/clarkema/nomad
# Mike Clarke <clarkema@clarkema.org>

function fuzzy-git-branch
{
    local picked

    if git rev-parse --is-inside-work-tree >& /dev/null; then
        picked=$(
            (
                git tag;
                git branch --all --format='%(refname:lstrip=2)'
            ) | ${NOMAD_PICKER:-fzf}
        )

        LBUFFER="${LBUFFER}$picked"
    fi
}
zle -N fuzzy-git-branch

