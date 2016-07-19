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
;; - idea-darkula-theme

;; org mode
(setq org-startup-indented t)

;; recent-files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Ctrl-x,c,v - cut, copy, paste
(cua-mode t)

(load-theme 'idea-darkula t)
