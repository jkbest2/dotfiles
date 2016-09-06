
;;MELPA Setup
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Org-mode --------------------------------------------------------------------
;; Load org-mode and org-mobile
(require 'org-install)
; (require 'calfw-org)
; (require 'org-mobile)

;; Set key combos for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Capture Templates
(setq org-capture-templates
 '(
   ("t" "To Do" entry (file+headline "~/Copy/org/todo.org" "To Do") 
    "** TODO %?\n SCHEDULED: %^t\n %U")
   ("d" "Deadline" entry (file+headline "~/Copy/org/todo.org" "To Do") 
    "** TODO %?\n DEADLINE: %^t\n %U")
   ("a" "Appointment" entry (file+headline "~/Copy/org/appts.org" "Appointments")
    "** %?\n %^T\n %U")
   ("j" "Journal" entry (file+datetree "~/Copy/org/journal.org")
    "* %?\n Entered on: %U\n %i\n %a\n")
   ("g" "Groceries" checkitem (file "~/Copy/org/groceries.org")
    " [ ] %?")
  )
)

;; Custom agenda views
(setq org-agenda-custom-commands
      '(("p" tags "project")
	("n" todo "NEXT")
	("w" todo "WAIT")
	("d" "Daily"
	 ((todo "NEXT")
	  (agenda "d")))))

;; Add TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "START(s@)" "WAIT(w@/!)" "|" "DONE(d!)" "CNCL(c@)")))
(setq org-log-done 'note)

;; Don't inherit :project: tag
(setq org-tags-exclude-from-inheritance (quote ("project")))

;; Location of org files on local system for MobileOrg
; (setq org-directory "~/Copy/org")

;; ;; Custom-set variables --------------------------------------------------------
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(TeX-view-program-list (quote (("Emacs" "emacsclient %o"))))
;;  '(TeX-view-program-selection
;;    (quote
;;     (((output-dvi style-pstricks)
;;       "dvips and gv")
;;      (output-dvi "xdvi")
;;      (output-pdf "Evince")
;;      (output-html "xdg-open"))))
;;  '(ansi-color-names-vector
;;    ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
;;  '(custom-enabled-themes (quote (solarized-dark)))
;;  '(custom-safe-themes
;;    (quote
;;     ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "3e821b86f0e789e1e26e943d6897d6cc5786e12da8b8233e5ad6c6308ea6ed82" default)))
;;  '(doc-view-continuous t)
;;  '(ess-R-font-lock-keywords
;;    (quote
;;     ((ess-R-fl-keyword:modifiers . t)
;;      (ess-R-fl-keyword:fun-defs . t)
;;      (ess-R-fl-keyword:keywords . t)
;;      (ess-R-fl-keyword:assign-ops . t)
;;      (ess-R-fl-keyword:constants . t)
;;      (ess-fl-keyword:fun-calls . t)
;;      (ess-fl-keyword:numbers)
;;      (ess-fl-keyword:operators . t)
;;      (ess-fl-keyword:delimiters . t)
;;      (ess-fl-keyword:=)
;;      (ess-R-fl-keyword:F&T)
;;      (ess-R-fl-keyword:%op%))))
;;  '(global-visual-line-mode t)
;;  '(inferior-R-font-lock-keywords
;;    (quote
;;     ((ess-S-fl-keyword:prompt . t)
;;      (ess-R-fl-keyword:messages . t)
;;      (ess-R-fl-keyword:modifiers . t)
;;      (ess-R-fl-keyword:fun-defs . t)
;;      (ess-R-fl-keyword:keywords . t)
;;      (ess-R-fl-keyword:assign-ops . t)
;;      (ess-R-fl-keyword:constants . t)
;;      (ess-fl-keyword:matrix-labels . t)
;;      (ess-fl-keyword:fun-calls . t)
;;      (ess-fl-keyword:numbers)
;;      (ess-fl-keyword:operators . t)
;;      (ess-fl-keyword:delimiters . t)
;;      (ess-fl-keyword:=)
;;      (ess-R-fl-keyword:F&T))))
;;  '(inhibit-startup-screen t)
;;  '(org-adapt-indentation t)
;;  '(org-agenda-files
;;    (quote
;;     ("~/Copy/org/someday.org" "~/Copy/org/reference.org" "~/Copy/org/weather.org" "~/Copy/math614/math614.org" "~/Copy/org/school.org" "~/Dropbox/Boreholes/boreholes.org" "~/Copy/math401/math401.org" "~/Copy/stat693/stat693.org" "~/Copy/Graduate_Programs.org" "~/Copy/org/personal.org" "~/Copy/org/todo.org" "~/Copy/org/appts.org" "~/Copy/org/journal.org")))
;;  '(org-agenda-ndays 7)
;;  '(org-agenda-skip-deadline-if-done t)
;;  '(org-agenda-skip-scheduled-if-done t)
;;  '(org-agenda-start-on-weekday nil)
;;  '(org-agenda-time-grid
;;    (quote
;;     ((daily today require-timed remove-match)
;;      #("----------------" 0 16
;;        (org-heading t))
;;      (800 1000 1200 1400 1600 1800 2000))))
;;  '(org-startup-indented t)
;;  '(org-stuck-projects (quote ("+LEVEL=2/-DONE" ("NEXT") nil ""))))
 ;; Should indent each line to the level of the headline by default - not working yet.

;; Add calendar options
(setq calendar-latitude 47.6)
(setq calendar-longitude -122.3)
(setq calendar-location-name "Seattle, WA")
(setq org-agenda-include-diary t)


;; Add R Markdown mode ---------------------------------------------------------
;; (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(require 'poly-R)
(require 'poly-markdown)
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; ;; Custom faces ----------------------------------------------------------------
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-date ((t (:foreground "dodger blue" :underline t))))
;;  '(org-todo ((t (:foreground "firebrick" :weight bold)))))
;; 
;; ;; Find previous (next) command in history matching current line
;; (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
;; (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
;; 
;; ESS Setup -------------------------------------------------------------------
;; (load "~/ESS/lisp/ess-site")
;;(setq inferior-julia-program-name "/usr/bin/julia")

;; (setq ess-smart-S-assign-key ";")
; (ess-toggle-S-assign nil)
; (ess-toggle-S-assign nil)
; (ess-toggle-underscore nil)

;; Add key combos to ESS mode
(require 'key-combo)
(global-key-combo-mode)

(defvar key-combo-ess-default
  (append key-combo-common-default
      '((">"  . (" > " " %>% "))
        ("$"  . ("$" " %$% "))
        ("<>" . " %<>% ")
        ("*"  . ("*" " * "))
        ("^"  . ("^" " ^ "))
        ("/"  . ("/" " / "))
        ("~" . " ~ ")
        (":" . (":" "::" ":::"))
        (":="  . " := ") ; data.table
        ;; ("inn" . " %in% ") ; doesn't work
        ("->"  . " -> ")
	("-" . "-")))) ; fix - to have no spaces by default

(key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
               'ess-key-combo-load-default
               key-combo-ess-default)

;; ESS font-lock defaults don't highlight enough. I like colors.
;; '(ess-R-font-lock-keywords
;;   (quote
;;     ((ess-R-fl-keyword:modifiers . t)
;;      (ess-R-fl-keyword:fun-defs . t)
;;      (ess-R-fl-keyword:keywords . t)
;;      (ess-R-fl-keyword:assign-ops . t)
;;      (ess-R-fl-keyword:constants . t)
;;      (ess-fl-keyword:fun-calls . t)
;;      (ess-fl-keyword:numbers)
;;      (ess-fl-keyword:operators . t)
;;      (ess-fl-keyword:delimiters . t)
;;      (ess-fl-keyword:=)
;;      (ess-R-fl-keyword:F&T)
;;      (ess-R-fl-keyword:%op%))))

;; Upcoming ess release (September 2015) should have:
; (ess-set-style 'RStudio)
;;(ess-offset-argument prev-call)	   

;; Define appropriate indentation rules
;; (add-to-list 'ess-style-alist
;;              '(my-style
;;                (ess-indent-level . 4)
;;                (ess-first-continued-statement-offset . 2)
;;                (ess-continued-statement-offset . 0)
;;                (ess-brace-offset . -4)
;;                (ess-expression-offset . 4)
;;                (ess-else-offset . 0)
;;                (ess-close-brace-offset . 0)
;;                (ess-brace-imaginary-offset . 0)
;;                (ess-continued-brace-offset . 0)
;;                (ess-arg-function-offset . 4)
;;            (ess-arg-function-offset-new-line . '(4))
;;                ))
;; 
;; (setq ess-default-style 'my-style)

;; Use Monoid in smaller size
; (set-default-font "Monoid-9")
(add-to-list 'default-frame-alist
             '(font . "Fira Mono for Powerline 11"))

;; Delete selected text when highlighting
(delete-selection-mode t)

;; Quit asking me where to start the R process!
(setq ess-ask-for-ess-directory nil)

;; Open R (iESS) in own frame
;; (setq inferior-ess-own-frame t)

;; Activate line numbers for certain modes
;; (defun my-c-mode-hook () 
;;   (linum-mode 1)) 
;; (add-hook 'c-mode-hook 'my-c-mode-hook) 

;; (defun my-python-mode-hook () 
;;   (linum-mode 1)) 
;; (add-hook 'python-mode-hook 'my-python-mode-hook) 

;; (defun my-ess-mode-hook ()
;;   (linum-mode 1))
;; (add-hook 'ess-mode-hook 'my-ess-mode-hook)

;; (defun my-markdown-mode-hook ()
;;   (linum-mode 1))
;; (add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;; (defun my-org-agenda-mode-hook ()
;;   (set-face-attribute 'default nil :height 120))
;;  (set-frame-font "Monoid-11" nil t))
;; (add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

(global-linum-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(setq-default fill-column 80)

;; Forecasts in org-mode??
;; (add-to-list 'load-path "~/.emacs.d/forecast.el")
;; (require 'forecast)
;; (setq forecast-latitude 64.857031
;;       forecast-longitude -147.825634
;;       forecast-city "Fairbanks"
;;       forecast-country "US"
;;       forecast-api-key "94e52d1399da086d006d1abf86ac68bf"
;;       forecast-language 'en
;;       forecast-units 'us)

;; Load the org-weather library ------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/org-weather")
;; (require 'org-weather)
;; ;; Set your location and refresh the data
;; (setq org-weather-location "College, AK")
;; ;; (defvar org-weather-api-url "http://api.openweathermap.org/data/2.5/forecast/daily?q=%s&mode=json&units=imperial&cnt=7")
;; ;; (defvar org-weather-format "%icon %desc, %tmin%tu to %tmax%tu, %p%pu, %h%hu, %s%su")
;; ;;(org-weather-refresh)

;; Powerline modeline ----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/powerline")
(require 'powerline)
(powerline-default-theme)

;; Ido mode --------------------------------------------------------------------
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Magit-status keybinding
(global-set-key (kbd "C-x g") 'magit-status)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray15")

;; PDF mode (rather than DVI-mode)
(setq TeX-PDF-mode t)

;; Show page breaks as horizontal lines
;;(global-page-break-lines-mode t)

(load "~/bin/ESS/lisp/ess-site")
;; (setq inferior-julia-program-name "/usr/bin/julia")
;; (require 'ess-julia-mode)

;; Enable evil mode
(require 'evil)
(evil-mode 1)

;; Enable R, Python, and Julia in Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   ; (julia . t)
   ))

;; Solarized Dark
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'solarized-dark t)
