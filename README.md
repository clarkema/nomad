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

 * `kco`   - `git checkout`
 * `kci`   - `git commit`
 * `kcia`  - `git commit --all`
 * `kcp`   - `git cherry-pick`
 * `kd`    - `git diff`
 * `kdc`   - `git diff --cached`
 * `kf`    - `git fetch`
 * `kl`    - `git log`
 * `klg`   - `git log --graph --all` with custom format
 * `kp`    - `git push`
 * `kpf`   - `git push -f`

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

 1. Create `$HOME/.nomad/local`
 2. Install Stow in `$HOME/.nomad/local/stow/stow-X.Y.Z`
 3. Re-exec your shell (`ez`)
 4. `$STOW_DIR/stow-X.Y.Z/bin/stow stow-X.Y.Z`
 5. Read the Stow documentation for the details of package installation,
    but in essence:

        $ ./configure --prefix $HOME/.nomad/local
        $ make
        $ make install prefix=$HOME/.nomad/local/stow/package-X.Y.Z
        $ stow package-X.Y.Z
    
NPM
---

NPM 'global' installations are installed in `$HOME/.npm-packages`

Stow and node.js
----------------

There are a few quirks when using node.js with GNU Stow, due to the
fact that node.js is itself a package manager.

To start with, you'll need a version of Python that supports the bz2
module.  To test if yours does, run `python -c 'import bz2'`.  If it
exits silently, all is well.  If not you might find yourself needing
to install libbzip2 and a custom version of Python.  To do this:

 1. Download and unpack the bzip2 source.
 2. Run the following in the source directory:

        $ make -f Makefile-libbz2_so
        $ make install PREFIX=$HOME/.nomad/local/stow/bzip2

 3. `stow bzip2`
 4. Build and install Python in the usual way; it should pick up
    on your local bzip2 installation.
 5. Once the `python -c 'import bz2` test is passing, download
    and unpack the node.js source.
 6. Build and install node.js as normal, *except* that instead of
    the 'split prefix' approach described in the main Stow section
    you should:

        $ configure --prefix=$HOME/.nomad/local/stow/node-x.y.z
        $ make
        $ make install

 7. `stow node-x.y.z`
 8. You should now be able to do 'global' installations, such as
    `npm install -g handlebars` and have it install to
    `$HOME/.nomad/local/stow/node-x.y.z/bin/handlebars`
 9. In summary, this results in node keeping everything neatly within
    the prefix it was configured with, *but* you need to re-Stow
    node every time you install a new package to create the
    new symlinks required.

