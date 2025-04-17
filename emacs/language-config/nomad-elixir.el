(use-package exunit)


(if (and (version<= "29" emacs-version)
         (string-match-p "TREE_SITTER" system-configuration-features))
    (progn
      (use-package elixir-ts-mode
        :defer t)
      (unless
          (treesit-language-available-p 'elixir)
        (message "Elixir tree-sitter grammar not installed.\nRun M-x elixir-ts-install-grammar.")))
  (use-package elixir-mode
    :defer t
    :hook
    (elixir-mode . yas-minor-mode)
    (elixir-mode . exunit-mode)))


(use-package which-key)
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
 ; :diminish lsp-mode
  :hook
  (elixir-mode . lsp-deferred)
  (elixir-ts-mode . lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-elixir-local-server-command (executable-find "elixir-ls"))
  )

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-hover t))

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
