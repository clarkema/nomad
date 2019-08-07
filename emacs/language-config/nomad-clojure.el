(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (fci-mode)
              (show-paren-mode)
              (paredit-mode)
              (subword-mode)       ; Useful for camel-case tokens, like names of
                                   ; Java classes
              (clj-refactor-mode 1))))

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t
  :after (clojure-mode))

(use-package cider
  :ensure t
  :defer t
  :after (clojure-mode)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  :bind (:map clojure-mode-map
              ("C-M-r" . cider-refresh)
              ("C-c u" . cider-user-ns)
         :map cider-mode-map
              ("C-c u" . cider-user-ns)))

(push '("*cider-error*" :height 20) popwin:special-display-config)
;(push '("*cider-test-report*" :height 20) popwin:special-display-config)
(push '("*cider-macroexpansion*" :height 20) popwin:special-display-config)

(use-package clj-refactor
  :ensure t
  :defer t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; go right to the REPL buffer when it's finished connecting
;(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
;(setq cider-show-error-buffer t)
;(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
;(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
;(setq cider-repl-wrap-history t)

(setq cljr-warn-on-eval nil)
;; enable paredit in your REPL
;(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
;(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
;(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
;(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
;(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))
