;;;
;;; Zettlekasten setup
;;;

;; Temporarily disable cl package deprecation warnings until deft catches
;; up.  See https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

(defun lfn-deft-toggle ()
  (interactive)
  (if (string= (buffer-name) "*Deft*")
      (kill-buffer nil)
    (deft)))

(use-package deft
  :straight t
  :bind ("<f7>" . lfn-deft-toggle)
  :commands (lfn-deft-toggle)
  :config
  (setq deft-extensions '("org")
        deft-default-extension "org"
        deft-directory "~/zk"
        deft-auto-save-interval 0
        deft-use-filter-string-for-filename t
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase))))


;;; org-roam test below here

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/notes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . left)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))
