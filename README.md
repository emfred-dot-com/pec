# Introduction

Welcome to `pec`, my personal Emacs configuration. The acronym `pec` is an
__ambigunym__, meaning it _could_ expand to:

- Personal Emacs Configuration

but could also expand to any of these other accurate descriptors:

- Powerful Emacs Configuration
- Portable Emacs Configuration
- Phenomenal Emacs Configuration
- Perfect Emacs Configuration  
[...]

`pec` is all about providing users with a menu of excellent options; so, of
course its name will do that too ;).

# Structure

`pec` is composed of a set of Emacs Lisp files that provide configurations and
extensions under a certain topic or category. These files are named according to
the pattern `*-setup.el`, where `*` is substituted with the name of the topic.
Some examples are:

- `terminal-setup.el`, which contains my configurations for creating and
  switching between terminal emulator buffers.

- `mail-setup.el`, which contains my configurations for reading and sending
  email within Emacs, and for related features like email address completion.

- `builtin-options-setup.el`, wherein I set a bunch of Emacs's built-in options
  (i.e. variables) to my desired values.

These `*-setup.el` files (of which there are currently 50) are the main
substance of `pec`. The only other files in the repo are:

- `init.el`, the Emacs startup file. This simply acts as a driver which loads
  all of the `*-setup.el` files.

- `README.md`, which you're reading right now.

- Miscellanea: `.gitignore` and the `LICENSE`.

# Installation

All of the Emacs Lisp (`.el`) files in the `pec` repo are intended to live
within the top level of your Emacs configuration directory (called the
`user-emacs-directory` in Emacs itself). This is traditionally `~/.emacs.d`, but
can also be elsewhere if you'd like (though note that `pec` simply keeps the
default of `~/.emacs.d`). If you're just getting started with Emacs and don't
have an Emacs configuration directory yet, then you can run the following shell
command, which will create your Emacs configuration directory at `~/.emacs.d`
with all of the `pec` Emacs Lisp files in it:

```sh
git clone https://github.com/emfred-dot-com/pec.git ~/.emacs.d
```

If, on the other hand, you already have an Emacs configuration of your own, then
sourcing `*-setup.el` files of your choosing from your existing `init.el` file
is likely the way to go. Try sourcing these as early in `init.el` as possible
so that your settings (e.g. keybindings) can overwrite my settings in case of a
conflict (instead of the other way around).

# Usage

`pec` has a bunch of stuff in it, and is probably overwhelming to fresh eyes.
Some files are very well commented; others, not so much. Since `pec` started off
as a purely personal project, thorough documentation wasn't a day-1 priority.
I'll continue trying to fill the gaps when I can find the time, but for now, the
following notes will hopefully be helpful:

The main way that Emacs (and hence `pec` as well) exposes functionality to users
is via keybindings, i.e. things you can type in order to make Emacs do something
for you. A simple example of this is "C-n", which tells Emacs to move your
cursor down to the next line in the buffer. The configuration files in `pec`
create many new keybindings, almost always via an Emacs Lisp macro that I
created called `keybind` (or `keybinds` (plural) if more than one is being
defined). So, a good way to find out what functionality `pec` provides is to
look through the `*-setup.el` files for things of the form
`(keybind [KEY] [COMMAND])`. For example, if you see:

```elisp
(keybind "C-z ." eshell-other-window)
```

Then you know that typing "C-z ." (Control+"z", followed by ".") will run the
command `eshell-other-window` (which happens to create an Emacs Lisp shell
session and make it pop up in a split window that's adjacent to your current
one). This example is from `shell-setup.el` -- there are, of course, many other
examples.

Searching for keybindings in this way, and then trying them out, is probably the
best way to become familiar with `pec`.

# Questions?

If you have any of the following gifts for me:

- Questions
- Bug reports
- Feature requests

Please consider [creating an
issue](https://github.com/emfred-dot-com/pec/issues) in this repository, which I
will try to address as soon as I can.

If you have anything else you'd like to communicate to me, you can find ways to
contact me via the following page on my website:
[https://emfred.com/about/contact/](https://emfred.com/about/contact/).

Thank you for reading!

\- Eric @ [emfred.com](https://emfred.com)
