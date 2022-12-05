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
  (org-roam-directory "~/zk")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
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

;;; Test hackery.  Create a buffer listing all 'book' sources known in org-roam
;;; Unlikely to work well on large databases
(defun slipper-list-sources ()
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*slipper list*"))
    (erase-buffer)
    (insert (propertize "slipper - list of sources:\n" 'face '(:weight bold)))
    (let ((all-nodes (org-roam-node-list)))
      (cl-loop for node in all-nodes
               do (let ((type (cdr (assoc "TYPE" (org-roam-node-properties node)))))
                    (if (equal type "Book")
                        (insert (format "- Node: %s \n" (org-roam-node-title node)))))))))
