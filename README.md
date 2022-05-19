# Sensible default settings for Emacs

Emacs is a terrific editor, but its out-of-the-box experience isn't great. Some
of its default settings feel a bit clunky. `sensible-defaults.el` aspires to
help new users configure some of those settings in a simple and modular way
(i.e., it's just a file of functions you can call).

The `sensible-defaults.el` file contains a number of function definitions, any
of which can be called from your `~/.emacs` or `~/.emacs.d/init.el` file.

`sensible-defaults.el` includes settings for:

* Ensuring that files end with newlines,
* Always enabling syntax highlighting,
* Increasing the garbage collection threshold,
* Defaulting line-length to 80 characters,
* Creating parent directories after saving a deeply nested file,
* Making `dired` file sizes human-readable,
* AND MORE!

For good or ill, sensible-defaults doesn't deal with any external packages. It
only tweaks settings built into the editor itself. If you wanna configure
packages, you're on your own.

## Getting started

Emacs reads its initial configuration from either `~/.emacs.d/init.el` or
`~/.emacs`. We suggest using a `.emacs.d` directory so you can keep your
configuration code in more than one file.

Create a directory called `sensible-defaults` containing `sensible-defaults.el`
(or just clone this repository somewhere).

Add the file to your `load-path` and `require` it in your `init.el`, then browse
through `sensible-defaults.el` and decide which settings you'd like to enable.
Call those functions in your `init.el`. That's it!

``` emacs
(add-to-list 'load-path "path/to/sensible-defaults")
(require 'sensible-defaults)
```

## An Example

Suppose you want to use every setting and every keybinding suggested by
`sensible-defaults`. Your `init.el` should contain the following code:

```emacs
(add-to-list 'load-path "path/to/sensible-defaults")
(require 'sensible-defaults)

(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
```

## A More Selective Example

Maybe you just want to increase the garbage collection threshold, show matching
parentheses, and bind the `Home` and `End` keys correctly. Kind of a weird set
of choices, but whatevs, it's easy:

```emacs
(add-to-list 'load-path "path/to/sensible-defaults")
(require 'sensible-defaults)

(sensible-defaults/increase-gc-threshold)
(sensible-defaults/show-matching-parens)
(sensible-defaults/bind-home-and-end-keys)
```

That's it!

## Non-Default Settings

The `sensible-defaults/backup-to-temp-directory` setting isn't enabled by
default (i.e., it isn't included in `sensible-defaults/use-all-settings`), since
it could lead to unexpected data loss in some cases.

If you're the sort of person who doesn't rely much on backups and saves
reflexively, though, this might be a perfectly fine choice for you.

If you choose to enable it, just call it like any of the other functions:

``` emacs
(sensible-defaults/backup-to-temp-directory)
```
