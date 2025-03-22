# bash widgets for git commands -- part of Nomad.
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

	if [[ -n "$picked" ]]; then
	    # Sanitise the picked string
	    picked=$(printf "%s" "$picked" | tr -d '\n')

	    # And insert it into the current command line
	    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$picked${READLINE_LINE:$READLINE_POINT}"
	    READLINE_POINT=$((READLINE_POINT + ${#picked}))
	fi
    fi
}

