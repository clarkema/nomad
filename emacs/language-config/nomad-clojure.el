(add-hook 'clojure-mode-hook
          (lambda () ()
            (fci-mode)
            (show-paren-mode)
            (paredit-mode)
            (subword-mode)         ; Useful for camel-case tokens, like names of
                                        ; Java classes
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c C-m")))

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;;;;
;; Cider
;;;;

(push '("*cider-error*" :height 20) popwin:special-display-config)
(push '("*cider-test-report*" :height 20) popwin:special-display-config)
(push '("*cider-macroexpansion*" :height 20) popwin:special-display-config)

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
;(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
;(setq cider-show-error-buffer t)
;(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
;(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
;(setq cider-repl-wrap-history t)

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

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

