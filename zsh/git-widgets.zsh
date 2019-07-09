# zsh widgets for git commands -- part of Nomad.
# https://github.com/clarkema/nomad
# Mike Clarke <clarkema@clarkema.org>

function fuzzy-git-branch
{
    local picked

    picked=$(
        (
            git tag;
            git branch --all --format='%(refname:lstrip=2)'
        ) | ${NOMAD_PICKER:-fzf}
    )

    LBUFFER="${LBUFFER}$picked"
}
zle -N fuzzy-git-branch

