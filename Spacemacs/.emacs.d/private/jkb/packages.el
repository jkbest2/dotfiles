;; Personal packages all in one layer.
(defconst jkb-packages
  '(stan-mode
    stan-snippets
    (tmb :location local)
    (admb :location local)
    olivetti
    polymode
    poly-markdown
    poly-R
    speed-type))

(defun jkb/init-stan-mode ()
  (use-package stan-mode
   :defer t))

(defun jkb/init-stan-snippets ()
  (use-package stan-mode
   :defer t
   :init
  (add-hook 'stan-mode-hook '(lambda () (yas-minor-mode)))))

(defun jkb/init-tmb ()
  (use-package tmb))

(defun jkb/init-admb ()
  (use-package admb
    :mode ("\\.tpl" . admb-mode)))

(defun jkb/init-olivetti ()
  (use-package olivetti
    :defer t
    :init
    (progn
      (setq olivetti-body-width 80)
      (spacemacs/set-leader-keys "to" 'olivetti-mode))))

(defun jkb/init-poly-markdown ()
  (use-package poly-markdown
    :defer t))

(defun jkb/init-poly-R ()
  :defer t)

(defun jkb/init-polymode ()
  (use-package polymode
  :mode (("\\.Rmd" . rmd-mode))
  :init
  (progn
    (defun rmd-mode ()
      "Polymode for R Markdown files"
      (interactive)
      (require 'poly-R)
      (require 'poly-markdown)
      (R-mode)
      (poly-markdown+r-mode))
    ))
  )

(defun jkb/init-speed-type ()
  (use-package speed-type
    :defer t
    :init (spacemacs/set-leader-keys "aT" 'speed-type-text)))

(defun jkb/post-init-olivetti ()
  (when (version< emacs-version "26.1")
    (add-hook 'olivetti-mode-hook (lambda ()
                                    (linum-mode -1)
                                    (spacemacs/toggle-fringe-off)))))
