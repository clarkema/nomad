(use-package elixir-mode
  :ensure t
  :hook
  (elixir-mode . yas-minor-mode))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  :init (add-to-list 'exec-path "/home/clarkema/.software/elixir-lsp"))

(use-package yasnippet
  :ensure t)

(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))
