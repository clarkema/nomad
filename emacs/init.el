;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")
;;                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

(defvar my-packages
  '(company
    cider
    helm-rg
    helm-cider
    restclient-helm
    popwin
    clj-refactor
    ))

(straight-use-package 'popwin)
(straight-use-package 'company)
(straight-use-package 'fill-column-indicator)
(straight-use-package 'olivetti)
(straight-use-package 'win-switch)
(straight-use-package 'deadgrep)

(use-package projectile)

(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/language-config")

;; Prevent Custom from modifying this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(autoload 'qlang-mode "qlang-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.qkk\\'" . qlang-mode))
;;;
;;; Fundamentals
;;;
(unless (and (display-graphic-p)
             (string-equal system-type "darwin"))
  (menu-bar-mode -1))
(blink-cursor-mode -1)
(tool-bar-mode -1)
(if (display-graphic-p)
  (scroll-bar-mode -1))
(xterm-mouse-mode)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)

(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

(desktop-save-mode 1)
;; Stop super-annoying default popup window behaviour
(require 'popwin)
(popwin-mode 1)

(push '("*json-path*" :height 5) popwin:special-display-config)

;;; Using the exec-path-from-shell package works better in complicated
;;; situations with Nix, but the non-package version below might be useful
;;; in bootstrapping situations where we need to get started with straight.el,
;;; so I'm keeping it around for now.

;;(defun set-exec-path-from-shell-PATH ()
;;  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;    (setenv "PATH" path-from-shell)
;;    (setq exec-path (split-string path-from-shell path-separator))))
;;
;;(when window-system (set-exec-path-from-shell-PATH))

(use-package exec-path-from-shell
  :demand
  :commands exec-path-from-shell-initialize
  :if (not (memq system-type '(cygwin windows-nt)))
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

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

;;; Re-enable disabled commands
(put 'narrow-to-region 'disabled nil)

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
  :straight nil
  :config

  ;; Enable extensions like C-x C-j (dired-jump)
  (require 'dired-x)

  (add-hook 'dired-mode-hook
            (lambda ()
              (local-set-key "u" 'dired-up-directory))))

(add-hook 'Info-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))

;;; Third-party packages

(use-package git-gutter
  :defer t
  :init
  :config
  (setq git-gutter:disabled-modes '(org-mode asm-mode image-mode)
        git-gutter:update-interval 1
        git-gutter:window-width 2
        git-gutter:ask-p nil))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine
  :straight t
  :defer t)

(use-package flycheck
  :straight t
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-check-syntax-automatically '(mode-enabled save new-line)))

(use-package paredit
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;(add-to-list 'load-path "~/.emacs.d/vendor/async")
(use-package helm)
(global-set-key (kbd "C-;") 'helm-M-x)
;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;(global-set-key (kbd "C-x b") 'helm-buffers-list)
;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;(global-set-key (kbd "C-x r b") 'helm-bookmarks)
;(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000))

(use-package vertico-prescient
  :straight t
  :after vertico
  :config
  (vertico-prescient-mode +1))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

(use-package consult
  :straight t)

(setq-default indent-tabs-mode nil)

;;; Mira DPI: 206
;;; Framework laptop DPI: 201
;;; LG 32" & LG DualUp ratio: 5

(defun frame-params (frame)
  (if window-system
      (let* ((displays (display-monitor-attributes-list frame))
             (display (car (cl-remove-if-not (lambda (d)
                                               (memq frame (assq 'frames d)))
                                             displays)))
             (px-width (nth 3 (assq 'geometry display)))
             (mm-width (nth 1 (assq 'mm-size display)))
             (inch-width (/ mm-width 25.4)))
        (/ px-width inch-width))))

(set-face-attribute 'default nil :font "Source Code Pro 10")
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro 10")
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.3)

(defun show-frame-params (&optional frame)
  (interactive)
  (if window-system
      (let* ((frame (or frame (selected-frame)))
             (dpi (frame-params frame)))
        (message "DPI: %s" dpi))))

;;; Older font options for reference
;;;  (set-frame-parameter frame 'font "Iosevka 16")
;;;  (set-frame-parameter frame 'font "Source Code Pro 16")
;;;  (set-frame-parameter frame 'font "Source Code Pro 10")
;;;  (set-frame-parameter frame 'font "Menlo 12")
(defun fontify-frame (&optional frame)
  (interactive)
  (if window-system
      (let* ((frame (or frame (selected-frame)))
             (dpi (frame-params frame)))
        (cond ((> dpi 200)
               (set-frame-parameter frame 'font "Source Code Pro 9"))
              ((> dpi 150)              ; Paul's ultrawide with 1.5 scale
               (set-frame-parameter frame 'font "Source Code Pro 10"))
              ((> dpi 137)
               ;; LG Evo under sway
               (set-frame-parameter frame 'font "Source Code Pro 14"))
              ((> dpi 110)              ; Paul's ultrawide without scaling
               (set-frame-parameter frame 'font "Source Code Pro 9"))))))

;(add-hook 'window-configuration-change-hook 'fontify-frame)

(load "nomad-lisp")
                                        ;(load "nomad-clojure")
(load "nomad-elixir")
(load "nomad-raku")

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
  :straight t)

(use-package xquery-mode
  :defer t
  :straight t)

(use-package git-timemachine
  :defer t
  :straight t)

(use-package terraform-mode
  :defer t
  :straight t)

(use-package nix-mode
  :defer t
  :straight t)

(load (expand-file-name "init-org" user-emacs-directory))
(load (expand-file-name "init-zk" user-emacs-directory))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))

(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

;; (use-package solarized-theme
;;   :straight t
;;   :config
;;   (setq solarized-distinct-fringe-background nil
;;         solarized-use-less-bold t
;;         solarized-high-contrast-mode-line t))
;  (load-theme 'solarized-dark t)

  ;; (if (display-graphic-p)
  ;;     (progn
  ;;       (if (string-equal system-type "darwin")
  ;;           (load-theme 'solarized-light t)
  ;;         (load-theme 'solarized-dark t))
  ;;       (set-cursor-color "#ff0000"))))

(defun dired-truncate-files ()
  (dolist (marked (dired-get-marked-files))
    (shell-command (concat "echo > " marked))))

;;; Colour themes
(straight-use-package 'gruvbox-theme)
;(straight-use-package 'solarized-theme)

(defun theme (name)
  (load-theme name t)
  (set-cursor-color "#ff0000"))

(if (and (display-graphic-p)
         (string-equal system-type "darwin"))
    (theme 'solarized-light)
  (theme 'gruvbox-dark-medium))

(setq world-clock-list
      '(("America/Los_Angeles" "West Coast")
        ("America/New_York" "East Coast")
        ("Atlantic/Stanley" "Falklands")
        ("UTC" "UTC")
        ("Europe/London" "London")
        ("Indian/Mahe" "Seychelles")
        ("Australia/Perth" "Perth")
        ("Australia/Melbourne" "Melbourne")
        ("Pacific/Auckland" "New Zealand")))
