---
title: "Dotfiles"
author: "John Best"
---

# Editors

## Vim

Primary configuration is in `.vimrc`. It's nothing too special, but enables most of the standard options. The `.vim` directory contains the standard structure, along with Vim 8-style package directories, which contain git submodules for each plugin.

## Emacs

My `.emacs` file is bloated and ugly, but it was working fairly well last time I used it. Some dependencies like `org-weather` will not be included here, so I should probably remove them from `.emacs`.

# X-related

## `.Xresources`

Options for the `URxvt`terminal:

- font: Hack, size 9
- boldfont: Hack Bold, size 9
- letterSpace: -1, too far apart otherwise (and other fonts like Fira Mono don't work at all.
- scrollBar: no scroll bar
- intensityStyles: false (not sure why)
- fading: Don't fade when focus lost

Options for `Xft` fonts:
- dpi: Laptop screen in ~130dpi
- antialias
- rgba: none (possibly related to font spacing in `URxvt`)
- hinting: true
- hintstyle: hintslight (also font spacing related?)

Colors: Solarized

## `.Xmodmap`

Set right-Alt to Compose key, and Caps-lock to Escape (doesn't work on external keyboards though).

# `.tmux.conf`

Set 256-color compatibility, lower escape time (suggestion from Neovim), and enable mouse mode.
- 
