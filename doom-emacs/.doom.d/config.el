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
(setq doom-font (font-spec :family "FantasqueSansMono Nerd Font Mono" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

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

;; Define org-ref package
;; (use-package! org-ref
;;   :after org
;;   :init
;;   (setq org-ref-completion-library 'org-ref-ivy-cite)
;;   :config
  ;; (setq org-ref-default-bibliography '("~/Dropbox/literature/library.bib")
  ;;       org-ref-pdf-directory '("~/Dropbox/literature")
  ;;       ;; Tell org-ref to let helm-bibtex find notes for it
  ;;       org-ref-notes-function
  ;;       (lambda (thekey)
  ;;         (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
  ;;           (bibtex-completion-edit-notes
  ;;            (List (car (org-ref-get-bibtex-key-and-file thekey))))))))

(use-package! org-ref
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-default-bibliography '("~/Dropbox/literature/library.bib")
        org-ref-pdf-directory '("~/Dropbox/literature")
        ;; Tell org-ref to let helm-bibtex find notes for it
        org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
             (list (car (org-ref-get-bibtex-key-and-file thekey))))))))

;; Bibtex setup
(setq bibtex-completion-bibliography "~/Dropbox/literature/library.bib"
      bibtex-completion-library-path "~/Dropbox/literature"
      bibtex-completion-notes-path "~/Dropbox/org/notes/literature"
      reftex-default-bibliography '("~/Dropbox/literature/library.bib"))

;;; Key bindings
;; Save all unsaved open buffers (SPC-fS in Spacemacs)
(map! :leader :n "fA" 'evil-write-all)

;;; General options
(set-fill-column 80)

;;; IRC settings
(set-irc-server! "irc.freenode.net"
                 `(:use-tls t
                            :port 6697
                            :server-buffer-name "freenode"
                            :user ,(+pass-get-user "chat/freenode")
                            :nick ,(+pass-get-user "chat/freenode")
                            :sasl-username ,(+pass-get-user "chat/freenode")
                            :sasl-password (lambda (&rest _) (+pass-get-secret "chat/freenode"))
                            ;; :sasl-password ,(+pass-get-secret "chat/freenode")
                            :channels (:after-auth
                                       "##bayes"
                                       "#emacs"
                                       ;; "#guile"
                                       ;; "#guix"
                                       "#julia"
                                       "#org-mode"
                                       "#R"
                                       "##statistics")))
;;; From https://github.com/jorgenschaefer/circe/wiki/Configuration
;; Use channel name in prompt
(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat "[" (buffer-name) "]")
                       'face 'circe-prompt-face)
           " ")))
;; Align nicks and messages
(setq circe-format-say "{nick:15s} | {body}"
      circe-format-self-say "{nick:15s} > {body}")

;;; Org-mode
(after! org
  (setq org-directory "~/Dropbox/org"
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
                             (org-agenda-files :maxlevel 3))
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
  (setq deft-directory "~/Dropbox/org/notes"
        deft-recursive t
        deft-extension '("org", "md", "Rmd")
        deft-new-file-format "%Y%m%d%H%M%S"
        deft-use-filename-as-title nil))

;;; +roam
(after! org-roam
  (setq org-roam-directory "~/Dropbox/org/notes"))


;;; Email (notmuch and message-mode)
(after! notmuch
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first :search-type tree)
          (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first :search-type nil)
          (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first :search-type tree)
          (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first :search-type nil)
          (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
          (:name "all mail" :query "*" :key "a" :sort-order newest-first))
        +notmuch-mail-folder "~/mail/jkbest.uw"
        +notmuch-sync-backend 'gmi
        +notmuch-sync-command "gmi sync"))
;; Use sendmail (msmtp here) to send mail
(setq mail-host-address "uw.edu"
      message-auto-save-directory "~/mail/drafts"
      message-default-mail-headers "Cc: \nBcc: John Best <jkbest@uw.edu> \n"
      message-kill-buffer-on-exit t
      message-send-mail-function 'message-send-mail-with-sendmail
      message-signature t
      message-signature-file "~/.signature")

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
(setq lsp-julia-default-environment "~/.julia/environments/v1.5")

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
  :config
  ;; No configuration options as of now.
  )

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
(use-package ac-stan
  :load-path "path-to-your-directory/ac-stan/"
  ;; Delete the line below if using.
  :disabled t
  :hook (stan-mode . stan-ac-mode-setup)
  ;;
  :config)
  ;; No configuration options as of now.
