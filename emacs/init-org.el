;;;
;;; org mode setup
;;; See https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;;;
(straight-use-package 'org-super-agenda)
(require 'org)
;(require 'ox-taskjuggler)
(add-to-list 'org-modules 'org-habit t)
(setq diary-file "~/org/diary")
(setq org-agenda-include-diary t)
(setq org-hide-leading-stars nil)
(setq org-agenda-files '("~/org/lw" "~/org/lfn" "~/org/personal"))
(setq org-confirm-elisp-link-function nil)
(setq org-agenda-custom-commands
      '(("c" "Syntacog"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High priority unfinished tasks:")))
          (todo "PROJ"
                ((org-agenda-overriding-header "Active projects")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLOCKED" "WAITING" "PROJ")))))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting:")))
          (todo "BLOCKED"
                ((org-agenda-overriding-header "Blocked tasks:"))))
         ((org-agenda-files (list "~/org/lfn/stc.org"))))
        ("l" "Lambda summary"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High priority unfinished tasks:")))
          (todo "PROJ"
                ((org-agenda-overriding-header "Active projects")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLOCKED" "WAITING" "PROJ")))))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting:")))
          (todo "BLOCKED"
                ((org-agenda-overriding-header "Blocked tasks:"))))
         ((org-agenda-files (list "~/org/lfn"))))
        ("p" "Personal summary"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High priority unfinished tasks:")))
          (todo "PROJ"
                ((org-agenda-overriding-header "Active projects")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLOCKED" "WAITING" "PROJ")))))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting:")))
          (todo "BLOCKED"
                ((org-agenda-overriding-header "Blocked tasks:"))))
         ((org-agenda-files (list "~/org/personal"))))
        ("s" "Shopping"
         ((search "Shopping")))))

(add-hook 'org-mode-hook
          (lambda ()
            (olivetti-mode)
            (olivetti-set-width 100)
            (display-line-numbers-mode -1)))

(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "S-<f6>") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

(org-super-agenda-mode -1)
(setq org-super-agenda-groups
      '(;; Each group has an implicit boolean OR operator between its selectors.
        (:name "Today"                  ; Optionally specify section name
                                        ;    :time-grid t           ; Items that appear on the time grid
               :todo "TODAY")           ; Items that have this TODO keyword
        (:name "Important12"
               ;; Single arguments given alone
                                        ;    :tag "bills"
               :priority "A")
        (:name "Misc"
               :todo "NEXT")
        ;; Set order of multiple groups at once
        (:order-multi (2 (:name "Shopping in town"
                                ;; Boolean AND group matches items that match all subgroups
                                :and (:tag "shopping" :tag "@town"))
                         (:name "Food-related"
                                ;; Multiple args given in list with implicit OR
                                :tag ("food" "dinner"))
                         (:name "Personal"
                                :habit t
                                :tag "personal")
                         (:name "Space-related (non-moon-or-planet-related)"
                                ;; Regexps match case-insensitively on the entire entry
                                :and (:regexp ("space" "NASA")
                                              ;; Boolean NOT also has implicit OR between selectors
                                              :not (:regexp "moon" :tag "planet")))))
        (:order-multi (3 (:name "LW"
                                :and (:file-path "lw.org"))))

        (:order-multi (3 (:name "Personal"
                                :and (:file-path "personal"))))
        ;; Groups supply their own section names when none are given
        (:name "Waiting"
               :todo "WAITING" :order 8) ; Set order of this section
        (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
               ;; Show this group at the end of the agenda (since it has the
               ;; highest number). If you specified this group last, items
               ;; with these todo keywords that e.g. have priority A would be
               ;; displayed in that group instead, because items are grouped
               ;; out in the order the groups are listed.
               :order 9)
        (:priority<= "B"
                     ;; Show this section after "Today" and "Important", because
                     ;; their order is unspecified, defaulting to 0. Sections
                     ;; are displayed lowest-number-first.
                     :order 1)
        ;; After the last group, the agenda will display items that didn't
        ;; match any of these groups, with the default order position of 99
        ))

(defun current-week-monday ()
  (concat
   "WC "
   (if (equal (format-time-string "%u") "1")
       (org-read-date nil nil "")
     (org-read-date nil nil "-mon"))))

(setq org-id-link-to-org-use-id 'create-if-interactive)
;;; This is not ideal because it requires the weekly headings to exist already
;;; and they are 'hard-coded' into the variable after this setq has run.  If the
;;; emacs instance lives over a week boundary it will maintain the old date
(setq org-capture-templates
      `(("i" "Interrupt" entry (file+olp "~/org/elc/elc.org" ,(current-week-monday) "Interrupts")
         "* TODO %?\n"
         :clock-in t
         :clock-keep t)
        ("a" "Ad-hoc" entry (file+olp "~/org/elc/elc.org" ,(current-week-monday) "Ad-hoc")
         "* TODO %?\n")
        ("n" "Ad-hoc, clock in" entry (file+olp "~/org/elc/elc.org" ,(current-week-monday) "Ad-hoc")
         "* IN-PROGRESS %?\n"
         :clock-in t
         :clock-keep t)
        ("p" "Personal inbox" entry (file+headline "~/org/personal/personal.org" "Short term")
         "* TODO %?\n")
        ("s" "Shopping" entry (file+headline "~/org/personal/personal.org" "Shopping")
         "* TODO %?\n")
        ("q" "Quality of life" entry (file+headline "~/org/lfn/stc.org" "Quality of life")
         "* TODO %?\n")

        ("w" "Work")
        ("wd" "Daily" plain (file+datetree "~/org/lfn/plan-work.org")
         (file "~/org/templates/review.org")
         :immediate-finish t
         :jump-to-captured t)))

;;;
;;; org-present config adapted from
;;; https://systemcrafters.net/emacs-tips/presentations-with-org-present/
;;;

(defun my/org-present-start ()
  (org-present-read-only)
  (hide-mode-line-mode +1)
  (face-remap-add-relative 'default :inherit 'variable-pitch :height 1.5)
  (face-remap-add-relative 'org-code nil :inherit 'fixed-pitch :height 1.55)
  (face-remap-add-relative 'org-verbatim nil :inherit 'fixed-pitch :height 1.55)
  (face-remap-add-relative 'org-block :inherit 'fixed-pitch :height 1.25)
  (face-remap-add-relative 'org-block-begin-line :inherit 'fixed-pitch :height 0.7)
  (face-remap-add-relative 'org-block-end-line :inherit 'fixed-pitch :height 0.7)
  (setq header-line-format " ")
  (setq org-hide-emphasis-markers t)
  (org-display-inline-images))

(defun my/org-present-stop ()
  (org-present-read-write)
  (hide-mode-line-mode -1)
  (setq-local face-remapping-alist nil)
  (setq header-line-format nil)
  (setq org-hide-emphasis-markers nil)
  (org-remove-inline-images)
  (org-present-show-cursor))

(require 'org-faces)
(use-package org-present
  :defer t)

(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-stop)
