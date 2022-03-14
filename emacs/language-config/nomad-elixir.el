(use-package elixir-mode
  :defer t
  :hook
  (elixir-mode . yas-minor-mode))

(use-package which-key)
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
 ; :diminish lsp-mode
  :hook
  (elixir-mode . lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-hover nil))

(use-package lsp-treemacs)

(use-package yasnippet)

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package js
  :custom
  (js-indent-level 2))
