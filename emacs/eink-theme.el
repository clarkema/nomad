;;; eink-theme.el --- An Emacs colour theme suitable for monochrome eink displays -*- lexical-binding: t -*-

;; Copyright (c) 2023 Mike Clarke

;; Author: Mike Clarke <clarkema@clarkema.org>
;; (current maintainer)
;;
;; URL: http://github.com/clarkema/emacs-theme-eink
;; Version: 0.0.0

;; Package-Requires: ((autothemer "0.2"))

(eval-when-compile
  (require 'cl-lib))

(deftheme eink
 "A colour theme for monochrome eink displays")

(let ((white-1 "#ffffff") (black-1 "#000000")
      (gray-1 "#dddddd"))
  (custom-theme-set-faces
   'eink
   `(default ((t (:foreground ,black-1 :background ,white-1))))
   '(fringe ((t (:foreground nil :background nil))))
   `(vertical-border ((t (:foreground ,black-1))))
   `(vertico-current ((t (:foreground ,black-1, :background ,gray-1))))

   ;; org-mode
   `(org-block-begin-line ((t (:background ,gray-1))))
   `(org-block-end-line ((t (:background ,gray-1))))
   `(org-block ((t (:background ,gray-1))))

   ;; org headings
   `(org-level-1 ((t (:foreground ,black-1 :background ,white-1 :bold t))))
   `(org-level-2 ((t (:foreground ,black-1 :background ,white-1 :bold t))))

   ;; dired
   `(dired-directory ((t (:foreground ,black-1 :background ,white-1))))

   ;; line numbers
   `(line-number ((t (:foreground ,black-1 :background ,white-1))))
   `(linum ((t (:foreground ,black-1 :background ,white-1))))))


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'eink)

;;; eink-theme.el ends here
