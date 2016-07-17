;; emacs package system
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; packages:
;; - better-defaults
;; - inf-ruby

;; org mode
(setq org-startup-indented t)

;; recent-files
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

