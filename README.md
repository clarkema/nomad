     _   _                           _
    | \ | | ___  _ __ ___   __ _  __| |
    |  \| |/ _ \| '_ ` _ \ / _` |/ _` |
    | |\  | (_) | | | | | | (_| | (_| |
    |_| \_|\___/|_| |_| |_|\__,_|\__,_|


Nomad
=====

Nomad is my personal dotfile repository.  Much has been 'borrowed' from
the many other excellent dotfile repositories out there, but then broken
down and reassembled to suit my personal tastes, needs, and working
style.

To paraphrase the YADR documentation: "Vim is the best editor.  Zsh is
the best shell.  Solarized is the best colour scheme..." _BUT_ I work as
a consultant and contractor.  I might find myself working on my OS X
laptop, a modern FreeBSD installation, a Debian machine, and a crufty
old server with minimal facilities within the space of a day.  This
repository is my attempt to make the best of whatever is available, in
order to provide myself with the most productive environment possible.

Shell
=====

Aliases
-------

### Git

 * `g`     - `git`
 * `gco`   - `git checkout`
 * `gci`   - `git commit`
 * `gcia`  - `git commit --all`
 * `gcp`   - `git cherry-pick`
 * `gd`    - `git diff`
 * `gdc`   - `git diff --cached`
 * `gf`    - `git fetch`
 * `gl`    - `git log`
 * `glg`   - `git log --graph --all` with custom format
 * `gp`    - `git push`
 * `gpf`   - `git push -f`

### Misc. Utilities

 * `ncd` - Create and change into a new directory in one step

Vim
===

Vim Keymaps
-----------

### Command-T

 * `,t` - fuzzy match filenames.
 * `,b` - fuzzy match buffers.

Vim Plugins
-----------

 * LustyJuggler
 * LustyExplorer
 * Ack
 * Fugitive
 * Command-T
 * Gundo
 * NERDCommenter
 * Syntastic
 * Tabular
 * Tagbar
 * Ultisnips
 * Abolish
 * Coffeescript
 * Solarized
 * Repeat
 * Surround

GNU Stow
========

When working on a remote system, I often find that it doesn't have
all the tools and libraries that I might wish for.  Assuming I have
root and the tool I want is packaged for the relevant distribution
or OS, no problem.  However, if I don't have admin, or I want to
install the software using options other than those with which it
was packaged (this is often the case with Vim) then I end up
compiling it by hand.

The problem with this approach is the lack of package management;
over time you lose track of what belongs to what and upgrades and
uninstallation become difficult.

[GNU Stow](http://www.gnu.org/software/stow/) solves the problem by
automatically managing a symlink farm; allowing you to install each
package in its own directory and then merging them together into a
shared tree.

Nomad doesn't come with Stow, but does support it if you need it.

To get started:

 1. Create `$HOME/.local`
 2. Install Stow in `$HOME/.local/stow/stow-X.Y.Z`
 3. Re-exec your shell (`ez`)
 4. `$STOW_DIR/stow-X.Y.Z/bin/stow stow-X.Y.Z`
 5. Read the Stow documentation for the details of package installation,
    but in essence:

        $ ./configure --prefix $HOME/.local
        $ make
        $ make install prefix=$HOME/.local/stow/package-X.Y.Z
        $ stow package-X.Y.Z
    



