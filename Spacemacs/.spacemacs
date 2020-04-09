;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(auto-completion
     ;; better-defaults
     bibtex
     common-lisp
     csv
     (deft :variables
       deft-directory "~/Dropbox/org/notes"
       deft-recursive t
       deft-extension '("org" "md" "Rmd")
       deft-new-file-format "%Y%m%d%H%M%S"
       deft-use-filename-as-title nil)
     docker
     emacs-lisp
     emoji
     ;; (erc :variables
     ;;      erc-enable-sasl-auth t
     ;;      erc-autojoin-timing 'ident
     ;;      erc-join-buffer 'bury
     ;;      erc-server-list
     ;;      '(("irc.freenode.net"
     ;;         :port "6697"
     ;;         :ssl t
     ;;         :nick "isposdef"
     ;;         :password ,(password-read "chat/freenode")))
     ;;      erc-autojoin-channels-alist
     ;;      '((".*\\.freenode.net" "##statistics" "#R" "#julia"
     ;;         "#guile" "#emacs" "#org-mode" "##bayes" "#guix" "#org-mode")))
     ess
     (geolocation :variables
                  geolocation-enable-weather-forecast t)
     git
     github
     helm
     html
     imenu-list
     jkb ;; My layer with a bunch of packages
     julia
     latex
     lsp
     lua
     markdown
     ;; (multiple-cursors :variables
     ;;                   multiple-cursor-backend 'evil-mc)
     ;; neotree
     (notmuch :variables
              notmuch-spacemacs-layout-name "@Notmuch"
              notmuch-spacemacs-layout-binding "n"
              notmuch-archive-tags '("-inbox" "-unread"))
     (org :variables
          org-enable-org-journal-support t
          org-journal-date-format "%Y-%m-%d %A (W%W)"
          org-journal-time-prefix "** "
          org-journal-dir "~/Dropbox/org/journal"
          org-journal-enable-encryption t
          org-crypt-key "751707c9"
          org-tags-exclude-from-inheritance (quote ("crypt"))
          org-enable-hugo-support t
          ;; Start agenda on current day
          org-agenda-start-on-weekday nil)
     pass
     pdf
     rust
     scheme
     (shell :variables
            shell-default-shell 'vterm
            shell-default-height 30
            shell-default-position 'bottom)
     (spacemacs-layouts :variables
                        spacemacs-layouts-restricted-functions
                        '(spacemacs/window-split-double-columns
                          spacemacs/window-split-triple-columns
                          spacemacs/window-split-grid
                          spacemacs/toggle-golden-ratio))
     speed-reading
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)
     systemd
     themes-megapack
     treemacs
     twitter
     version-control
     yaml)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(academic-phrases
                                      epresent
                                      gnu-elpa-keyring-update
                                      org-noter
                                      org-plus-contrib
                                      solarized-theme)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(fill-column-indicator
                                    modus-vivendi-theme
                                    modus-operandi-theme)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-wombat-dark
                         solarized-gruvbox-dark
                         ;; doom-gruvbox
                         ;; doom-nord
                         ;; doom-opera
                         ;; subatomic
                         ;; doom-one
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 2)
   ;; dotspacemacs-mode-line-theme 'doom
   dotspacemacs-mode-line-theme '(all-the-icons :separator none)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   ;; dotspacemacs-default-font '("Monoid Nerd Font"
   ;;                             :size 10.0
   ;;                             :weight regular)
   ;;                             ;; :width wide)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers
   '(:relative t
     :visual nil
     :disabled-for-modes dired-mode
                         doc-view-mode
                         markdown-mode
                         org-mode
                         pdf-view-mode
                         text-mode
     :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code: This function is called at the
very end of Spacemacs startup, after layer configuration. Put
your configuration code here, except for variables that should be
set before packages are loaded."

  ;; Load secrets from separate .el file in ~
  (load-file "~/.sterces.el")

  (defun notmuch-sync-mail ()
    "Sync UW Gmail"
    (interactive)
    (async-shell-command "syncmail"))
  (spacemacs/set-leader-keys "aNr" 'notmuch-sync-mail)

  ;; Set default writeroom width to 90 (from 80) so that contents in org-buffers
  ;; that are visually indented don't get wrapped. 90 allows for a 5th level
  ;; heading to avoid wrapping. Should be plenty
  (setq writeroom-width 90)

  ;; Use sendmail (msmtp here) to send mail
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-kill-buffer-on-exit t)
  ;; Include self as Bcc to have record of sent messages
  (setq message-default-mail-headers "Cc: \nBcc: John Best <jkbest@uw.edu> \n")
  (setq message-auto-save-directory "~/mail/drafts")
  (setq mail-host-address "uw.edu")
  (setq message-signature t)
  (setq message-signature-file "~/.signature")

  ;; Default to holy-mode in vterm; too many modal modes simultaneously moding
  ;; with zsh in vim-mode
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))

  ;; Make sure that fill-column is sane
  (set-fill-column 80)

  ;; Don't turn on visual-line-mode for org-mode files; I'd prefer to use
  ;; auto-fill-mode for these
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

  ;; Turn on time in modeline, remove minor-modes in modeline
  (setq display-time-mode t)
  (setq display-time-day-and-date t)
  (setq display-time-string-forms
        '(dayname " " monthname " " day
                  " " 24-hours ":" minutes))
  ;; Remove minor modes in modeline to free up space for time
  ;; (spacemacs/toggle-mode-line-minor-modes-off)

  ;; org-mode settings
  (with-eval-after-load 'org
    ;; Make the buffers look prettier
    (setq org-startup-indented t)
    ;; Put these where deft can search them
    (setq org-agenda-files '("~/Dropbox/org"))
    (setq org-directory "~/Dropbox/org")
    ;; Fix refiling to use headers
    (setq org-refile-targets
          '((nil :maxlevel . 3)
            (org-agenda-files :maxlevel . 3)))
    ;; auto-fill-mode in org files. Don't expect to version control them and
    ;; looks nicer
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    ;; Set up non-journal encryption (key set above)
    (require 'org-crypt)
    (setq org-crypt-disable-auto-save t)
    (org-crypt-use-before-save-magic)
    (setq org-todo-keywords
          '((sequence "PROJ(p)" "BACK(b)" "|" "DONE(d!)" "CNCL(c@)")
            (sequence "TODO(t)" "BACK(b)" "NEXT(n)" "WAIT(w@!/!)" "|" "DONE(d!)" "CNCL(c@)")
            (sequence "SCHED(s)" "TENT(T)" "|" "CNCL(c@)")))
    (setq org-tag-alist '(("dissertation" . ?0)
                          ("ch1" . ?1)
                          ("ch2" . ?2)
                          ("ch3" . ?3)
                          ("ch4" . ?4)
                          (:newline . nil)
                          ;; Reading groups and seminars
                          ("quantsem" . ?q)
                          ("safssem" . ?f)
                          ("spacetime" . ?s)
                          ("puntlab" . ?p)
                          ;; Conferences
                          ("capam_m" . ?m)
                          ("wfc2020" . ?w)
                          ("isec2020" . ?i)
                          (:newline . nil)
                          ("extracurricular" . ?X)
                          ("zk" . ?z)
                          ("review" . ?r)
                          ;; StatR
                          ("statr" . ?R)
                          (:newline . nil)
                          ;; Personal group
                          ("kit" . ?k)
                          ("ada" . ?a)
                          ("john" . ?j)
                          ("dawson" . ?d)
                          ("family" . ?f)
                          (:newline . nil)))
    (setq org-agenda-use-time-grid nil)
    (setq org-default-notes-file (concat org-directory "/inbox.org"))
    (setq org-capture-templates
          '(("t" "To Do" entry
             (file+headline "Current.org" "Tasks")
             "** TODO %? \n %^{|DEADLINE|SCHEDULED}: %^t \n")
            ("b" "Backlog" entry
             (file+headline "Backlog.org" "Backlog")
             "** TODO %? \n")
            ("e" "Event" entry
             (file+headline "Current.org" "Events")
             "** %? \n %^T \n")
            ("n" "Note" entry
             (file+headline "inbox.org" "Notes")
             "** %? \n")))
    ;; org-babel setup
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (julia . t)
       (emacs-lisp . t)
       (ditaa . t)
       (plantuml . t)
       (dot . t)))
    (setq org-ditaa-jar-path "/usr/bin/ditaa")
    (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
    ;; When logging TODO state changes, keep times in drawer :LOGBOOK:, the
    ;; default
    (setq org-log-into-drawer t)
    (add-to-list 'org-modules 'org-habit t)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
                                              "h" 'org-habit-toggle-display-in-agenda)
    (setq org-archive-default-command 'org-archive-to-archive-sibling)
    (setq org-agenda-skip-additional-timestamps-same-entry t)
    (setq org-agenda-skip-archived-trees nil)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-reverse-note-order t)
    ;; I prefer to archive to sibling, which doesn't have a default key binding
    ;; in Spacemacs. Add ",sx" here.
    (spacemacs/set-leader-keys-for-major-mode
     'org-mode "sx" 'org-archive-to-archive-sibling))
  ;; (setq org-caldav-url 'google)
  ;; (setq org-caldav-calendar-id FILL IN HERE)
  ;; (setq org-caldav-inbox "~/Dropbox/notes/caldav-inbox.org")
  ;; (setq org-caldav-files "~/Dropbox/notes")
  ;; (setq org-icalendar-timezone "America/Los_Angeles"))

  ;; Activate pdf-tools
  ;; (pdf-tools-install)

  ;; Bibtex setup
  (setq bibtex-completion-bibliography "~/Dropbox/literature/library.bib")
  (setq bibtex-completion-library-path "~/Dropbox/literature")
  (setq bibtex-completion-notes-path "~/Dropbox/org/notes/literature")

  ;; Setup org-ref
  (setq reftex-default-bibliography '("~/Dropbox/literature/library.bib"))
  (setq org-ref-default-bibliography '("~/Dropbox/literature/library.bib")
        org-ref-pdf-directory "~/Dropbox/literature/")

  ;; Tell org-ref to let helm-bibtex find notes for it
  (setq org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
             (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

  ;; Make IRC quiet again
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  ;; R/ESS options
  (setq inferior-R-args "--no-save --no-restore-data")
  (setq ess-R-argument-suffix " = ")
  (ess-toggle-underscore nil)
  (setq ess-default-style 'RStudio)
  (add-hook 'ess-julia-mode-hook 'julia-mode)

  ;; Julia options
  (setq julia-repl-executable-records
        '((default "julia")
          (julia-0.7 "julia-0.7")
          (julia-1.2 "julia-1.2")
          (julia-1.3 "julia-1.3")))
  ;; Set numbec of threads as environmental variable for Julia v1.3+
  (setenv "JULIA_NUM_THREADS" "4")
  ;; Without this tries to open vim inside the julia-repl terminal process,
  ;; which gets messy quickly.
  (setenv "JULIA_EDITOR" "emacsclient")
  ;; Can be used to set e.g. number of processes with " -p 4". Can also be
  ;; called as a function to set switches on the fly.
  (setq julia-repl-switches "")

  ;; Default Spacemacs large file size too small (1Mb)
  (setq dotspacemacs-large-file-size 100)

  ;; Weather setup
  (setq sunshine-location "Seattle,WA,US")
  (setq sunshine-show-icons t)

  (setq spotify-transport 'connect
        spotify-player-status-playing-text "▶"
        spotify-player-status-paused-text "▮▮"
        spotify-player-status-stopped-text "■"
        spotify-player-status-not-shuffling-text ""
        spotify-player-status-shuffling-text "⤮"
        spotify-player-status-not-repeating-text ""
        spotify-player-status-repeating-text "🔁"
        spotify-player-status-format "%p %a/%t %s%r"
        spotify-player-status-truncate-length 15)

  ;; Use minibuffer for GPG password propmts
  ;; (setq epa-pinentry-mode 'loopback)
  ;; (pinentry-start)

  (with-eval-after-load 'flycheck
    (flycheck-define-checker todo-checker
      "A checker to list open TODOs, CITEs, or other annotations in
  a file."
      :command ("todo-checker" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ":" (message) line-end))
      :modes markdown-mode))

  ;; Add monitors to symon
  (setq symon-monitors '(symon-linux-cpu-monitor
                         symon-linux-memory-monitor
                         symon-linux-network-rx-monitor
                         symon-linux-network-tx-monitor
                         symon-linux-battery-monitor))

  (spacemacs|define-custom-layout  "@Spotify"
    :binding "m")

  (set-frame-name (concat "@" server-name)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#7ec98f")
 '(cua-normal-cursor-color "#8c8a85")
 '(cua-overwrite-cursor-color "#e5c06d")
 '(cua-read-only-cursor-color "#8ac6f2")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#2e2e2d")
 '(helm-completion-style (quote emacs))
 '(highlight-changes-colors (quote ("#e5786d" "#834c98")))
 '(highlight-symbol-colors
   (quote
    ("#53f14ae238f6" "#3dfc4d203fcc" "#58f348ab45e4" "#3ec3324f41e1" "#41574c3354ab" "#52464649390d" "#45ad48955232")))
 '(highlight-symbol-foreground-color "#989790")
 '(highlight-tail-colors
   (quote
    (("#2e2e2d" . 0)
     ("#3d454b" . 20)
     ("#3a463b" . 30)
     ("#404249" . 50)
     ("#4b4436" . 60)
     ("#4a4036" . 70)
     ("#4c3935" . 85)
     ("#2e2e2d" . 100))))
 '(hl-bg-colors
   (quote
    ("#4b4436" "#4a4036" "#4f4240" "#4c3935" "#3b313d" "#404249" "#3a463b" "#3d454b")))
 '(hl-fg-colors
   (quote
    ("#292928" "#292928" "#292928" "#292928" "#292928" "#292928" "#292928" "#292928")))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("CITE" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "cdc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fabd2f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#8ec07c"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#555556"))
 '(lsp-ui-doc-border "#989790")
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first :search-type tree)
     (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first :search-type nil)
     (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first :search-type tree)
     (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first :search-type nil)
     (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
     (:name "all mail" :query "*" :key "a" :sort-order newest-first))))
 '(nrepl-message-colors
   (quote
    ("#ffb4ac" "#ddaa6f" "#e5c06d" "#3d454b" "#e1e8e9" "#404249" "#7ec98f" "#e5786d" "#834c98")))
 '(objed-cursor-color "#fb4934")
 '(org-roam-directory "~/Dropbox/org/notes")
 '(package-selected-packages
   (quote
    (academic-phrases modus-vivendi-theme modus-operandi-theme chocolate-theme helm-pass password-store auth-source-pass treemacs-projectile treemacs-persp treemacs-magit treemacs-evil matrix-client rainbow-identifiers esxml tracking a anaphora helm-notmuch notmuch systemd all-the-icons-dired slime-company slime geiser common-lisp-snippets epresent org-roam pinentry yapfify utop tuareg caml seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe rbenv rake pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements ocp-indent ob-elixir nodejs-repl mvn minitest meghanada maven-test-mode lsp-python-ms lsp-java livid-mode skewer-mode live-py-mode json-navigator hierarchy js2-refactor multiple-cursors js2-mode js-doc importmagic epc concurrent deferred helm-pydoc groovy-mode groovy-imports gradle-mode flycheck-ocaml merlin flycheck-mix flycheck-credo dune cython-mode company-tern tern company-anaconda chruby bundler inf-ruby blacken auto-complete-rst anaconda-mode pythonic alchemist elixir-mode org-noter ox-hugo ox-gfm twittering-mode github-search github-clone gist gh marshal logito pcache forge ghub closql emacsql-sqlite emacsql erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks company-lua lua-mode gnu-elpa-keyring-update let-alist helm-gtags ggtags counsel-gtags org-caldav org-journal yaml-mode lsp-ui lsp-treemacs treemacs pfuture lv helm-lsp company-lsp dash-functional dockerfile-mode docker json-mode docker-tramp json-snatcher json-reformat web-mode web-beautify tagedit slim-mode scss-mode sass-mode pug-mode prettier-js impatient-mode helm-css-scss haml-mode emmet-mode counsel-css company-web web-completion-data add-node-modules-path zenburn-theme yasnippet-snippets xterm-color winum toc-org tao-theme speed-type solarized-theme slack circe seti-theme racket-mode racer poly-R poly-noweb paradox orgit org-ref pdf-tools tablist org-projectile org-mime org-download org-brain neotree naquadah-theme minimal-theme magit-svn lsp-julia link-hint kaolin-themes julia-repl inkpot-theme hl-todo highlight-indentation helm-xref helm-spotify-plus helm-make helm-bibtex parsebib gruvbox-theme google-translate git-timemachine git-link flyspell-correct-helm flyspell-correct eyebrowse expand-region evil-visual-mark-mode evil-surround evil-nerd-commenter evil-matchit evil-magit evil-goggles eval-sexp-fu eshell-prompt-extras emojify ht elfeed-goodies editorconfig dumb-jump dracula-theme doom-themes doom-modeline eldoc-eval diff-hl deft define-word darktooth-theme cyberpunk-theme counsel-projectile counsel swiper ivy color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized centered-cursor-mode cargo rust-mode browse-at-remote biblio biblio-core auto-yasnippet auto-compile apropospriate-theme alect-themes aggressive-indent ace-window ace-link avy auctex elfeed simple-httpd ess julia-mode anzu iedit smartparens flycheck company request projectile window-purpose imenu-list helm helm-core magit-popup magit transient git-commit with-editor alert f markdown-mode polymode spaceline dash powerline all-the-icons which-key use-package hydra async evil goto-chg org-plus-contrib zen-and-art-theme ws-butler white-sand-theme websocket volatile-highlights vi-tilde-fringe uuidgen undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treepy toxi-theme toml-mode tangotango-theme tango-plus-theme tango-2-theme symon sunshine sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stan-snippets spray spotify spinner spaceline-all-the-icons spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shrink-path shell-pop reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme purple-haze-theme professional-theme popwin poly-markdown planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el password-generator packed overseer organic-green-theme org-present org-pomodoro org-category-capture org-bullets open-junk-file omtose-phellack-theme olivetti oldlace-theme occidental-theme obsidian-theme oauth2 noflet noctilux-theme nameless mustang-theme multi-term multi move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lsp-mode lorem-ipsum log4e light-soap-theme key-chord jbeans-theme jazz-theme ir-black-theme indent-guide hungry-delete htmlize highlight-parentheses highlight-numbers highlight heroku-theme hemisu-theme helm-themes helm-swoop helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-gitignore helm-git-grep helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruber-darker-theme graphql grandshell-theme gotham-theme golden-ratio gnuplot gntp gitignore-templates gitconfig-mode gitattributes-mode git-messenger git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy font-lock+ flycheck-rust flycheck-pos-tip flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery faceup eziam-theme exotica-theme evil-visualstar evil-unimpaired evil-tutor evil-org evil-numbers evil-mc evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu ess-R-data-view espresso-theme eshell-z esh-help emoji-cheat-sheet-plus elisp-slime-nav elfeed-web elfeed-org dotenv-mode django-theme diminish darkokai-theme darkmine-theme darkburn-theme dakrone-theme csv-mode company-statistics company-emoji company-auctex column-enforce-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key badwolf-theme autothemer auto-highlight-symbol auto-dictionary anti-zenburn-theme ample-zen-theme ample-theme afternoon-theme ace-jump-mode ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(pos-tip-background-color "#2e2e2d")
 '(pos-tip-foreground-color "#989790")
 '(rustic-ansi-faces
   ["#282828" "#fb4934" "#8ec07c" "#fabd2f" "#268bd2" "#fb2874" "#83a598" "#ebdbb2"])
 '(send-mail-function (quote mailclient-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#8ac6f2" "#2e2e2d" 0.2))
 '(term-default-bg-color "#292928")
 '(term-default-fg-color "#8c8a85")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ffb4ac")
     (40 . "#f2adba4b8d2d")
     (60 . "#ec03bd377d5a")
     (80 . "#e5c06d")
     (100 . "#d12ac1a59a9a")
     (120 . "#c48bc297b092")
     (140 . "#b559c3a0c660")
     (160 . "#a283c4c2dc29")
     (180 . "#8ac6f2")
     (200 . "#8974c713d0ff")
     (220 . "#87dac796c08f")
     (240 . "#8568c814b01f")
     (260 . "#8220c88d9fa0")
     (280 . "#7ec98f")
     (300 . "#90a0c182b224")
     (320 . "#9814bd85c375")
     (340 . "#9e81b95bd4ba")
     (360 . "#a4b5e6"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#292928" "#2e2e2d" "#4f4240" "#ffb4ac" "#3d454b" "#8ac6f2" "#4b4436" "#e5c06d" "#404249" "#a4b5e6" "#4c3935" "#e5786d" "#3a463b" "#7ec98f" "#8c8a85" "#73726e")))
 '(xterm-color-names
   ["#2e2e2d" "#ffb4ac" "#8ac6f2" "#e5c06d" "#a4b5e6" "#e5786d" "#7ec98f" "#e7e4da"])
 '(xterm-color-names-bright
   ["#292928" "#ddaa6f" "#6a6965" "#73726e" "#8c8a85" "#834c98" "#989790" "#f5f2e7"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#d2cfc6" :background "#292928")))))
)
