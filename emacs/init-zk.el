;;;
;;; Zettlekasten setup
;;;

;;; org-roam test below here

(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography '("~/zk/references.bib")))

(use-package org-ref
  :defer t
  :after (org bibtex))

(require 'org-ref-helm)



(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)

;;; Force org-roam to use the slightly older default emacs-sqlite library.
;;; When emacs 29 (with built-in sqlite support) is released this can be
;;; revisited
;;; See discussion at https://github.com/org-roam/org-roam/issues/2485
(if (version< emacs-version "29")
    (progn
      (use-package sqlite3)
      (setq org-roam-database-connector 'sqlite-module))
  (setq org-roam-database-connector 'sqlite-builtin))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/zk")
  ;; This is the default db location, but expanded here to prevent
  ;; sqlite choking on a ~.
  ;; See https://github.com/org-roam/org-roam/issues/2488
  (org-roam-db-location (expand-file-name (locate-user-emacs-file "org-roam.db")))
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

;;; Allow abbreviated links from any org file to a ZK node in in the format
;;; [[zk:foo/bar]]
(autoload 'org-roam-node-from-title-or-alias "org-roam-node")

(add-to-list 'org-link-abbrev-alist
             '("zk" . "elisp:(slipper-jump \"%s\")"))

(defun slipper-jump (ref)
  (if-let ((node (org-roam-node-from-title-or-alias ref)))
      (progn
        (org-roam-node-open node)
        t)
    (user-error (concat "Node not found: " ref))))

;; (use-package consult
;;   :defer t)


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
    (insert (propertize "slipper - list of nodes:\n" 'face '(:weight bold)))
    (let ((all-nodes (org-roam-node-list)))
      (cl-loop for node in all-nodes
               do (let* ((parts (split-string (org-roam-node-title node) "[/]"))
                         (category (if (= (length parts) 1)
                                       "   "
                                     (car parts)))
                         (title (if (= (length parts) 1) (car parts) (cadr parts))))
                    (insert (format "- %s :: %s \n" (upcase category) title)))))))

;; Taken from https://www.reddit.com/r/emacs/comments/yn4hr8/how_to_properly_use_consultripgrep_to_search/
;; TODO Might be worth checking the other poster's notes package as well
(defun slipper-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 \
--path-separator / --smart-case --no-heading --with-filename \
--line-number --glob=!*~ --glob=!*#"))
        (consult-ripgrep "~/zk")))

(global-set-key (kbd "<f5>") 'slipper-search)
