(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(company
    cider
    solarized-theme
    monokai-theme
    win-switch
    helm-rg
    helm-cider
    restclient-helm
    popwin
    fill-column-indicator
    olivetti
    clj-refactor
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/language-config")

;;;
;;; Fundamentals
;;;
(menu-bar-mode -1)
(blink-cursor-mode -1)
(tool-bar-mode -1)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

(if (display-graphic-p)
    (progn
      (if (string-equal system-type "darwin")
	  (load-theme 'solarized-light t)
	(load-theme 'solarized-dark t)) 
      (set-cursor-color "#ff0000")
      (scroll-bar-mode 0))
  (progn
    (xterm-mouse-mode 1)
    ;(load-theme 'monokai t)
    ))


(desktop-save-mode 1)
;; Stop super-annoying default popup window behaviour
(require 'popwin)
(popwin-mode 1)

(push '("*json-path*" :height 5) popwin:special-display-config)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(add-hook 'after-init-hook 'global-company-mode)

(require 'fill-column-indicator)
(setq-default fill-column 80)
(column-number-mode 1)

(add-to-list 'load-path "~/.emacs.d/vendor/perl6-mode")

;; Don't treat the right-hand alt key on a Mac as Meta; this leaves it
;; free for use in key combinations such as # (alt-3) and € (alt-2).
(setq ns-right-alternate-modifier 'none)


(add-hook 'after-make-frame-functions
	  (lambda (frame) (set-frame-parameter frame 'cursor-color "#ff0000")))

;(require 'column-marker)
(setq ispell-program-name "aspell")
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(setq visible-bell nil)

(global-set-key "\C-xo" 'win-switch-dispatch)
(setq next-line-add-newlines nil)


;;;
;;; use-package setup
;;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;;; Built-in packages

(use-package calendar
  :config
  ;; Add ISO week numbers to calendar display
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :weight 'normal)

  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))

  (add-hook 'calendar-load-hook
            (lambda ()
              (calendar-set-date-style 'european))))

(use-package dired
  :config

  ;; Enable extensions like C-x C-j (dired-jump)
  (require 'dired-x))

;;; Third-party packages

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;;;
;;; org mode setup
;;; See https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(require 'org)
(add-to-list 'org-modules 'org-habit t)
(setq org-agenda-files '("~/org/"))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High priority unfinished tasks:")))
          (agenda "")
          (alltodo "")))))
(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;(add-to-list 'load-path "~/.emacs.d/vendor/async")
;(add-to-list 'load-path "~/.emacs.d/vendor/helm")
(require 'helm)
(require 'helm-config)

;; If you're seeing errors about helm-autoload, try running make
;; in the helm directory
(helm-mode 1)

(global-set-key (kbd "C-;") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-rg-active-arg-face ((t (:foreground "LightGreen"))))
 '(helm-rg-base-rg-cmd-face ((t (:background "#eee8d5" :foreground "#657b83"))))
 '(helm-rg-colon-separator-ripgrep-output-face ((t (:foreground "#fdf6e3" :background "#fdf6e3"))))
 '(helm-rg-directory-cmd-face ((t (:background "#eee8d5" :foreground "#cb4b16"))))
 '(helm-rg-directory-header-face ((t (:background "#eee8d5" :foreground "#cb4b16"))))
 '(helm-rg-error-message ((t (:background "#eee8d5" :foreground "#dc322f"))))
 '(helm-rg-file-match-face ((t (:foreground "LightGreen" :underline t))))
 '(helm-rg-inactive-arg-face ((t (:background "#eee8d5" :foreground "#657b83"))))
 '(helm-rg-line-number-match-face ((t (:foreground "#eee8d5" :background "#eee8d5"))))
 '(helm-rg-preview-line-highlight ((t (:background "LightGreen" :foreground "black"))))
 '(helm-rg-title-face ((t (:background "red"))))
 '(helm-selection ((t (:foreground "#f00" :background "#000"))))
 '(helm-source-header ((t (:background "green")))))



(setq-default indent-tabs-mode nil)

(defun fontify-frame (&optional frame)
  (interactive)
  (if window-system
    (let* ((frame (or frame (selected-frame)))
           (displays (display-monitor-attributes-list frame))
           (display (car (cl-remove-if-not (lambda (d)
                                             (memq frame (assq 'frames d)))
                                           displays)))
           (px-width (nth 3 (assq 'geometry display)))
           (mm-width (nth 1 (assq 'mm-size display))))
      ;; The '4' below is a magic number that is the cut-off point between
      ;; the built-in display on my Retina MBP and an external Zenscreen.
      ;; It's likely to need tweaking for other combinations.
      (if (> (/ px-width mm-width) 4)
        (set-frame-parameter frame 'font "Iosevka 16")
        ;(set-frame-parameter frame 'font "Source Code Pro 16")
        (set-frame-parameter frame 'font "Source Code Pro 10")
        ;;(set-frame-parameter frame 'font "Menlo 12")
        ))))

;(add-hook 'window-configuration-change-hook 'fontify-frame)

(load "nomad-lisp")
(load "nomad-clojure")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(package-selected-packages
   (quote
    (win-switch w3m solarized-theme sly restclient-helm popwin monokai-theme markdown-mode+ magit-popup magit macrostep helm-rg helm-cider ghub fill-column-indicator company color-theme-monokai color-theme-molokai clojure-mode-extra-font-locking clj-refactor ace-window))))
(use-package notmuch
  :defer t
  :bind (:map notmuch-show-mode-map
              ("d" . (lambda ()
                       "toggle deleted tag for message"
                       (interactive)
                       (if (member "deleted" (notmuch-show-get-tags))
                           (notmuch-show-tag (list "-deleted"))
                         (notmuch-show-tag (list "+deleted")))))
              :map notmuch-tree-mode-map
              ("d" . (lambda ()
                       "toggle deleted tag for message"
                       (interactive)
                       (if (member "deleted" (notmuch-tree-get-tags))
                           (notmuch-tree-tag-thread (list "-deleted"))
                         (notmuch-tree-tag-thread (list "+deleted")))))
              ("w" . (lambda ()
                       "consign to the news pile"
                       (interactive)
                       (notmuch-tree-tag-thread (list "-inbox" "+news"))))))

(use-package restclient
  :defer t
  :ensure t)
