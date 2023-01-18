;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John K Best"
      user-mail-address "jkbest@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "juliamono" :size 12)
;;       doom-variable-pitch-font (font-spec :family "Inter" :size 14))
;; (setq doom-font (font-spec :family "monospace" :size 12))
(setq doom-font (font-spec :family "FantasqueSansMono Nerd Font" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;; Key bindings
;; Save all unsaved open buffers (SPC-fS in Spacemacs)
(map! :leader :n "fA" 'evil-write-all)

;;; General options
(set-fill-column 80)

;;; Citar
(setq! citar-bibliography '("~/literature/library.bib"
                            "~/literature/calibre.bib")
       citar-library-paths '("~/literature/")
       citar-notes-paths '("~/org/notes/literature"))

;;; Org-mode
(after! org
  (setq org-directory "~/org"
        org-agenda-start-day nil
        org-agenda-start-on-weekday nil
        org-agenda-use-time-grid nil
        org-agenda-skip-additional-timestamps-same-entry t
        org-agenda-skip-archived-trees nil
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled nil
        org-crypt-key "751707c9"
        org-deadline-warning-days 7
        org-log-into-drawer t
        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3))
        org-reverse-note-order t
        org-startup-indented t
        org-startup-folded t
        org-todo-keywords
        '((sequence "PROJ(p)" "BACK(b)" "|" "DONE(d!)" "CNCL(c@)")
          (sequence "TODO(t)" "BACK(b)" "NEXT(n)" "WAIT(w@!/!)" "|" "DONE(d!)" "CNCL(c@)")
          (sequence "SCHED(s)" "TENT(T)" "|" "CNCL(c@)")))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (map! :localleader
        :map org-mode-map
        :nv "A" 'org-archive-to-archive-sibling))

;;; +journal
(setq org-journal-date-format "%Y-%m-%d %A (W%V)"
      ;; This setting doesn't work for some reason, so moved journal to default
      ;; `org/journal' location
      ;; org-journal-dir "~/Dropbox/journal/"
      org-journal-enable-encryption t
      org-journal-time-prefix "** ")

;;; deft
(after! deft
  (setq deft-directory "~/org/notes"
        deft-recursive t
        deft-extensions '("org" "md" "Rmd")
        deft-new-file-format "%Y%m%d%H%M%S"
        deft-use-filename-as-title nil))

;;; +roam
(after! org-roam
  (setq org-roam-directory "~/org/notes"))

;;; ESS
(after! ess
  (setq inferior-R-args "--no-save --no-restore-data"
        ess-R-argument-suffix " = "
        ess-set-style 'RStudio)
  (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode)))

;; page-break-line-mode
(global-page-break-lines-mode)

;; Move the mouse out of the way
(setq make-pointer-invisible nil)
(mouse-avoidance-mode 'animate)

;; Set default Julia environment
;; (setq lsp-julia-default-environment "~/.julia/environments/v1.6")
(setq julia-repl-executable-records
      '((default   "julia")                  ; in the executable path
        (julia-1.5 "julia-1.5")
        (julia-1.6 "julia-1.6")))
(after! vterm
  (after! julia-repl
    (julia-repl-set-terminal-backend 'vterm)))
(setq vterm-kill-buffer-on-exit nil)
(after! eglot-jl
  (setq eglot-jl-language-server-project eglot-jl-base))

;;; stan-mode.el
(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; eldoc-stan.el
(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config)
  ;; No configuration options as of now.
  

;;; flycheck-stan.el
(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
(use-package stan-snippets
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config)
;; No configuration options as of now.


;;; ac-stan.el (Not on MELPA; Need manual installation)
;; (use-package ac-stan
;;   :load-path "path-to-your-directory/ac-stan/"
;;   ;; Delete the line below if using.
;;   :disabled t
;;   :hook (stan-mode . stan-ac-mode-setup)
;;   ;;
;;   :config)
;;   ;; No configuration options as of now.

;; Set directories that contain projects
(after! magit
  (setq magit-repository-directories
        '(;; Directory containing project root directories
          ;; Projects I am developing
          ("~/dev/"        . 2)
          ;; Sources of other projects
          ("~/src/"        . 2)
          ;; Julia package development
          ("~/.julia/dev/" . 2))))

;; Then load known magit projects into projectile
(after! projectile
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects)))

;; Load quarto-mode
(use-package quarto-mode)

;; Load ox-gemini
(use-package ox-gemini)
