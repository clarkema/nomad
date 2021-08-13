;(add-to-list 'load-path "~/.emacs.d/vendor/sly")
;(require 'sly-autoloads)
;(require 'slime)
;(require 'slime-autoloads)

;(setq slime-contribs '(slime-fancy))
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;(add-hook 'sly-mode-hook 'sly-company-mode)
;(eval-after-load 'company
  ;'(add-to-list
    ;'company-backends '(sly-company)))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")
;(setq inferior-lisp-program "/Users/clarkema/.nomad/bin/lw-console")
;(setq sly-contribs '(sly-fancy))
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'show-paren-mode)
(add-hook 'lisp-mode-hook #'fci-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)

;; (use-package slime
;;   :defer t
;;   :straight t)
