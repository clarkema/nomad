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

(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/zk/references.bib")))

(use-package org-ref
  :defer t
  :after (org bibtex))

(require 'org-ref-helm)



(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)


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
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (slot . 0)
                 (window-width . 0.2)
                 (window-paramters . ((no-other-window 1)
                                      (no-delete-other-windows . t)))))
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section)))

(use-package org-roam-bibtex
  :defer t
  :after (org-roam helm-bibtex)
  :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
  :config
  (org-roam-bibtex-mode +1)
  (setq orb-insert-interface 'helm-bibtex))

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

(defun slipper-list-nodes ()
  (interactive)
  (with-current-buffer (switch-to-buffer-other-window (get-buffer-create "*slipper list*"))
    (erase-buffer)
    (insert (propertize "slipper - list of sources:\n" 'face '(:weight bold)))
    (let ((all-nodes (org-roam-node-list)))
      (cl-loop for node in all-nodes
               do (let* ((parts (split-string (org-roam-node-title node) "[/]"))
                         (category (if (= (length parts) 1)
                                       "   "
                                     (car parts)))
                         (title (if (= (length parts) 1) (car parts) (cadr parts))))
                    (insert (format "- %s :: %s \n" (upcase category) title)))))))
