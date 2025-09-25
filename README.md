# pec: Portable Emacs Configuration

Welcome to `pec`, my large (and growing) set of configurations for GNU Emacs.

# Installation

```sh
# assuming Emacs is installed, and ~/.emacs.d/ does not exist
mkdir ~/.emacs.d
cd ~/.emacs.d
git clone https://github.com/emfred-dot-com/pec.git
```

# Documentation

Inline documentation is common among the code in the `*-setup.el` files
(especially with custom functions and commands I've written, nearly all of which
have docstrings), but is not total. I'm planning to soon make an attempt at
fully documenting this configuration, to make it as easy as possible for new
users to understand what's happening. For now though, the names of the
`*-setup.el` files accurately reflect their contents, so, for example, to see
the setup I've done related to opening terminal emulators, `terminal-setup.el`
is the place to look. Ditto for `appearance-setup.el`, `mail-setup.el`, etc.
