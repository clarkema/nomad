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
