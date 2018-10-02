(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(company
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    solarized-theme
    win-switch
    helm-rg
    helm-cider
   ))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/language-config")

(tool-bar-mode 0)
(scroll-bar-mode 0)
(scroll-bar-mode 0)

(defalias 'yes-or-no-p 'y-or-n-p)
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
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;(add-to-list 'load-path "~/.emacs.d/vendor/sly")
;(require 'sly-autoloads)
(require 'slime)
(setq slime-contribs '(slime-fancy))
;(add-ho 'sly-mode-hook 'sly-company-mode)
;(eval-after-load 'company
;  '(add-to-list
;    'company-backends '(sly-company)))
;; Replace "sbcl" with the path to your implementation
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "/Users/clarkema/.nomad/bin/lw-console")
;(setq sly-contribs '(sly-fancy))
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'show-paren-mode)
(add-hook 'lisp-mode-hook #'fci-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)

;; Don't treat the right-hand alt key on a Mac as Meta; this leaves it
;; free for use in key combinations such as # (alt-3) and € (alt-2).
(setq ns-right-alternate-modifier 'none)

(load-theme 'solarized-light t)
(set-cursor-color "#ff0000")
(add-hook 'after-make-frame-functions
	  (lambda (frame) (set-frame-parameter frame 'cursor-color "#ff0000")))

(require 'column-marker)
(require 'flymake)
(setq ispell-program-name "aspell")
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(setq visible-bell nil)

(global-set-key "\C-xo" 'win-switch-dispatch)
(global-set-key (kbd "C-x g") 'magit-status)



(setq next-line-add-newlines t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (slamhound clj-refactor rnc-mode evil markdown-mode helm-rg helm-cider magit rainbow-delimiters restclient popwin helm paredit clojure-mode-extra-font-locking win-switch solarized-theme fill-column-indicator company))))
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

(require 'restclient)

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
        (set-frame-parameter frame 'font "Source Code Pro 12")
        ;;(set-frame-parameter frame 'font "Menlo 12")
        ))))

(add-hook 'window-configuration-change-hook 'fontify-frame)

(load "nomad-clojure")
